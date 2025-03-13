use std::marker::PhantomData;

use crate::{
    compose,
    control::{Applicative, Functor, TypeCtor},
    expression::{ExprCapable, Expression, FnType},
};

/// A composition of two type constructors.
#[derive(Debug, Clone)]
pub struct Compose<F: TypeCtor, G: TypeCtor>(PhantomData<(F, G)>);

impl<F: TypeCtor, G: TypeCtor> ExprCapable for Compose<F, G> {}
impl<F: TypeCtor, G: TypeCtor> TypeCtor for Compose<F, G> {
    type Apply<T: ExprCapable> = F::Apply<G::Apply<T>>;
}

impl<F: Functor, G: Functor> Functor for Compose<F, G> {
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        compose().apply(F::map()).apply(G::map())
    }
}

impl<F: Applicative, G: Applicative> Applicative for Compose<F, G> {
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        compose().apply(F::pure()).apply(G::pure())
    }

    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        compose().apply(F::map2()).apply(G::map2())
    }
}
