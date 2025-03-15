use crate::{
    data::Foldable,
    expression::{ExprCapable, Expression, FnType},
    function::{compose, constant, id},
    Expr, ExprType,
};

pub mod env;
pub mod identity;
pub mod state;
pub mod write;

/// Marks a type constructor, an abstraction for a generic type.
pub trait TypeCtor: ExprCapable {
    type Apply<T: ExprCapable>: ExprCapable;
}

pub trait TypeCtor2: ExprCapable {
    type Apply<A: ExprCapable, B: ExprCapable>: ExprCapable;
}

pub trait Zero: TypeCtor {
    fn zero<A: ExprCapable>() -> Expr!(Self::Apply<A>);
}

pub trait Functor: TypeCtor {
    fn map<A: ExprCapable, B: ExprCapable>() -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>);
}

pub trait ContraFunctor: TypeCtor {
    fn contramap<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((B => A) => Self::Apply<A> => Self::Apply<B>);
}

pub trait Applicative: Functor {
    fn pure<A: ExprCapable>() -> Expr!(A => Self::Apply<A>);

    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
    ) -> Expr!((A => B => C) => Self::Apply<A> => Self::Apply<B> => Self::Apply<C>);

    fn ap<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!(Self::Apply<ExprType!(A => B)> => Self::Apply<A> => Self::Apply<B>) {
        Self::map2().apply(id())
    }
}

pub trait Monad: Applicative {
    fn bind<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((A => Self::Apply<B>) => Self::Apply<A> => Self::Apply<B>);

    fn join<A: ExprCapable>() -> Expr!(Self::Apply<Self::Apply<A>> => Self::Apply<A>) {
        Self::bind().apply(id())
    }

    fn sequence<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!(Self::Apply<B> => Self::Apply<A> => Self::Apply<B>) {
        compose().apply(Self::bind()).apply(constant())
    }

    // (f <=< g) a = f =<< g a
    fn kleisli<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
    ) -> Expr!((B => Self::Apply<C>) => (A => Self::Apply<B>) => A => Self::Apply<C>) {
        compose().apply(compose()).apply(Self::bind())
    }
}

pub trait MonadFix: Monad {
    fn mfix<A: ExprCapable>() -> Expr!((A => Self::Apply<A>) => Self::Apply<A>);
}

pub trait Comonad: Functor {
    fn extract<A: ExprCapable>() -> Expr!(Self::Apply<A> => A);

    fn extend<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((Self::Apply<A> => B) => Self::Apply<A> => Self::Apply<B>);

    fn duplicate<A: ExprCapable>() -> Expr!(Self::Apply<A> => Self::Apply<Self::Apply<A>>) {
        Self::extend().apply(id())
    }

    // (f =<= g) wa = f (g <<= wa)
    fn cokleisli<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
    ) -> Expr!((Self::Apply<B> => C) => (Self::Apply<A> => B) => Self::Apply<A> => C) {
        Expression::new(FnType::new(|f| {
            FnType::new(|g| f.compose(Self::extend().apply(g)).eval())
        }))
    }
}

pub trait Traversable: Functor + Foldable {
    fn traverse<F: Applicative, A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((A => F::Apply<B>) => Self::Apply<A> => F::Apply<Self::Apply<B>>);

    fn sequence<F: Applicative, A: ExprCapable>(
    ) -> Expr!(Self::Apply<F::Apply<A>> => F::Apply<Self::Apply<A>>) {
        Self::traverse::<F, _, _>().apply(id())
    }
}
