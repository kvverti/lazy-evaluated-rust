use std::marker::PhantomData;

use crate::{
    control::{Applicative, Functor, TypeCtor},
    expression::{ExprCapable, Expression, FnType},
};

/// A composition of two functors, which is also a functor. If both are applicative, the composition is also applicative.
#[derive(Debug, Clone)]
pub struct Compose<F: TypeCtor, G: TypeCtor>(PhantomData<(F, G)>);

impl<F: TypeCtor, G: TypeCtor> ExprCapable for Compose<F, G> {}
impl<F: TypeCtor, G: TypeCtor> TypeCtor for Compose<F, G> {
    type Apply<T: ExprCapable> = F::Apply<G::Apply<T>>;
}

impl<F: Functor, G: Functor> Functor for Compose<F, G> {
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        F::map().compose(G::map())
    }
}

impl<F: Applicative, G: Applicative> Applicative for Compose<F, G> {
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        F::pure().compose(G::pure())
    }

    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        F::map2().compose(G::map2())
    }
}
