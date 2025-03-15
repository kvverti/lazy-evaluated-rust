use crate::{
    data::Foldable,
    expression::{ExprCapable, Expression, FnType},
    fix,
    function::{compose, constant, flip, id},
    Expr,
};

use super::{Applicative, Comonad, Functor, Monad, MonadFix, Traversable, TypeCtor};

/// The identity monad.
#[derive(Debug, Clone)]
pub struct Identity;

impl ExprCapable for Identity {}
impl TypeCtor for Identity {
    type Apply<T: ExprCapable> = T;
}

impl Functor for Identity {
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        id()
    }
}

impl Applicative for Identity {
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        id()
    }

    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        id()
    }

    fn ap<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<Self::Apply<FnType<A, B>>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        id()
    }
}

impl Monad for Identity {
    fn bind<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, Self::Apply<B>>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        id()
    }

    fn join<A: ExprCapable>() -> Expression<FnType<Self::Apply<Self::Apply<A>>, Self::Apply<A>>> {
        id()
    }

    fn sequence<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<Self::Apply<B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        constant()
    }

    fn kleisli<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
    ) -> Expr!((B => Self::Apply<C>) => (A => Self::Apply<B>) => A => Self::Apply<C>) {
        compose()
    }
}

impl MonadFix for Identity {
    fn mfix<A: ExprCapable>() -> Expr!((A => Self::Apply<A>) => Self::Apply<A>) {
        fix()
    }
}

impl Foldable for Identity {
    fn foldr<A: ExprCapable, B: ExprCapable>() -> Expr!((A => B => B) => B => Self::Apply<A> => B) {
        flip()
    }

    fn foldl_strict<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((B => A => B) => B => Self::Apply<A> => B) {
        id()
    }
}

impl Traversable for Identity {
    fn traverse<F: Applicative, A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((A => F::Apply<B>) => Self::Apply<A> => F::Apply<Self::Apply<B>>) {
        id()
    }

    fn sequence<F: Applicative, A: ExprCapable>(
    ) -> Expr!(Self::Apply<F::Apply<A>> => F::Apply<Self::Apply<A>>) {
        id()
    }
}

impl Comonad for Identity {
    fn extract<A: ExprCapable>() -> Expr!(Self::Apply<A> => A) {
        id()
    }

    fn extend<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((Self::Apply<A> => B) => Self::Apply<A> => Self::Apply<B>) {
        id()
    }

    fn duplicate<A: ExprCapable>() -> Expr!(Self::Apply<A> => Self::Apply<Self::Apply<A>>) {
        id()
    }

    fn cokleisli<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
    ) -> Expr!((Self::Apply<B> => C) => (Self::Apply<A> => B) => Self::Apply<A> => C) {
        compose()
    }
}
