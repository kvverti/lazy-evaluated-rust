use crate::{
    Expr, ExprType, data::Foldable, expression::{Expression, FnType}, function::{compose, constant, flip, id},
};

use super::{Applicative, Comonad, Functor, Monad, MonadFix, Traversable, TypeCtor};

/// The identity monad.
#[derive(Debug, Clone)]
pub struct Identity;

impl Expr for Identity {}
impl TypeCtor for Identity {
    type Apply<T: Expr> = T;
}

impl Functor for Identity {
    fn map<A: Expr, B: Expr>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        id()
    }
}

impl Applicative for Identity {
    fn pure<A: Expr>() -> Expression<FnType<A, Self::Apply<A>>> {
        id()
    }

    fn map2<A: Expr, B: Expr, C: Expr>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        id()
    }

    fn ap<A: Expr, B: Expr>(
    ) -> Expression<FnType<Self::Apply<FnType<A, B>>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        id()
    }
}

impl Monad for Identity {
    fn bind<A: Expr, B: Expr>(
    ) -> Expression<FnType<FnType<A, Self::Apply<B>>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        id()
    }

    fn join<A: Expr>() -> Expression<FnType<Self::Apply<Self::Apply<A>>, Self::Apply<A>>> {
        id()
    }

    fn sequence<A: Expr, B: Expr>(
    ) -> Expression<FnType<Self::Apply<B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        constant()
    }

    fn kleisli<A: Expr, B: Expr, C: Expr>(
    ) -> Expr!((B => Self::Apply<C>) => (A => Self::Apply<B>) => A => Self::Apply<C>) {
        compose()
    }
}

impl MonadFix for Identity {
    fn mfix<A: Expr>(f: ExprType!(A => Self::Apply<A>)) -> Expr!(Self::Apply<A>) {
        Expression::fix(f)
    }
}

impl Foldable for Identity {
    fn foldr<A: Expr, B: Expr>() -> Expr!((A => B => B) => B => Self::Apply<A> => B) {
        flip()
    }

    fn foldl_strict<A: Expr, B: Expr>(
    ) -> Expr!((B => A => B) => B => Self::Apply<A> => B) {
        id()
    }
}

impl Traversable for Identity {
    fn traverse<F: Applicative, A: Expr, B: Expr>(
    ) -> Expr!((A => F::Apply<B>) => Self::Apply<A> => F::Apply<Self::Apply<B>>) {
        id()
    }

    fn sequence<F: Applicative, A: Expr>(
    ) -> Expr!(Self::Apply<F::Apply<A>> => F::Apply<Self::Apply<A>>) {
        id()
    }
}

impl Comonad for Identity {
    fn extract<A: Expr>() -> Expr!(Self::Apply<A> => A) {
        id()
    }

    fn extend<A: Expr, B: Expr>(
    ) -> Expr!((Self::Apply<A> => B) => Self::Apply<A> => Self::Apply<B>) {
        id()
    }

    fn duplicate<A: Expr>() -> Expr!(Self::Apply<A> => Self::Apply<Self::Apply<A>>) {
        id()
    }

    fn cokleisli<A: Expr, B: Expr, C: Expr>(
    ) -> Expr!((Self::Apply<B> => C) => (Self::Apply<A> => B) => Self::Apply<A> => C) {
        compose()
    }
}
