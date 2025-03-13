use std::marker::PhantomData;

use crate::{
    data::{Monoid, Type},
    expression::{ExprCapable, Expression, FnType},
    Tup,
};

use super::{Applicative, Functor, TypeCtor};

/// The writer monad.
#[derive(Debug, Clone)]
pub struct Write<C: Type, T: TypeCtor>(PhantomData<(C, T)>);

impl<C: Type, T: TypeCtor> ExprCapable for Write<C, T> {}
impl<C: Type, T: TypeCtor> TypeCtor for Write<C, T> {
    type Apply<A: ExprCapable> = T::Apply<Tup!(C::Apply, A)>;
}

impl<C: Type, T: TypeCtor> Functor for Write<C, T> {
    // map f (c, a) = (c, f a)
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        todo!()
    }
}

impl<CO: Monoid, T: Applicative> Applicative for Write<CO, T> {
    // pure a = pure (empty, a)
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        T::pure().compose(Expression::new(FnType::new(|a| (CO::empty(), a))))
    }

    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        todo!()
    }
}
