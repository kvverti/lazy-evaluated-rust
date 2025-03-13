use crate::{
    data::Foldable,
    expression::ExprCapable,
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

pub trait Functor: TypeCtor {
    fn map<A: ExprCapable, B: ExprCapable>() -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>);
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

pub trait Traversable: Functor + Foldable {
    fn traverse<F: Applicative, A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((A => F::Apply<B>) => Self::Apply<A> => F::Apply<Self::Apply<B>>);

    fn sequence<F: Applicative, A: ExprCapable>(
    ) -> Expr!(Self::Apply<F::Apply<A>> => F::Apply<Self::Apply<A>>) {
        Self::traverse::<F, _, _>().apply(id())
    }
}
