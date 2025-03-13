use std::marker::PhantomData;

use crate::{
    constant,
    control::{Applicative, Functor, TypeCtor},
    expression::{ExprCapable, Expression, FnType},
    id,
};

use super::{Monoid, Type};

#[derive(Debug, Clone)]
pub struct Const<C: Type>(PhantomData<C>);

impl<C: Type> ExprCapable for Const<C> {}

impl<C: Type> TypeCtor for Const<C> {
    type Apply<T: ExprCapable> = C::Apply;
}

impl<C: Type> Functor for Const<C> {
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        constant().apply(id())
    }
}

impl<CO: Monoid> Applicative for Const<CO> {
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        constant().apply(CO::empty())
    }

    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        constant().apply(CO::append())
    }
}
