use std::marker::PhantomData;

use crate::{
    Expr, ExprType,
    control::{Applicative, Functor, TypeCtor},
    expression::{Expression, FnType},
};

/// A composition of two functors, which is also a functor. If both are applicative, the composition is also applicative.
#[derive(Debug, Clone)]
pub struct Compose<F: TypeCtor, G: TypeCtor>(PhantomData<(F, G)>);

impl<F: TypeCtor, G: TypeCtor> TypeCtor for Compose<F, G> {
    type Apply<T: Expr> = F::Apply<G::Apply<T>>;
}

impl<F: Functor, G: Functor> Functor for Compose<F, G> {
    fn map<A: Expr, B: Expr>()
    -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        F::map().compose(G::map())
    }
}

impl<F: Applicative, G: Applicative> Applicative for Compose<F, G> {
    fn pure<A: Expr>() -> Expression<FnType<A, Self::Apply<A>>> {
        F::pure().compose(G::pure())
    }

    fn map2<A: Expr, B: Expr, C: Expr>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        F::map2().compose(G::map2())
    }

    fn ap<A: Expr, B: Expr>()
    -> Expr!(Self::Apply<ExprType!(A => B)> => Self::Apply<A> => Self::Apply<B>) {
        F::map2().apply(G::ap())
    }
}
