use std::marker::PhantomData;

use crate::{
    constant,
    control::{Applicative, Functor, TypeCtor},
    expression::{DataExpr, ExprCapable, Expression, FnType},
};

use super::Monoid;

#[derive(Debug, Clone)]
pub struct Const<C, A> {
    data: Expression<C>,
    _ph: PhantomData<A>,
}

impl<C: ExprCapable, A: ExprCapable> ExprCapable for Const<C, A> {}
impl<C: ExprCapable, A: ExprCapable> DataExpr for Const<C, A> {
    fn destructure(v: Expression<Self>) -> Self {
        Self {
            data: Expression::lazy(|| v.eval().data.eval()),
            _ph: PhantomData,
        }
    }
}

impl<C: ExprCapable> TypeCtor for Const<C, ()> {
    type Apply<T: ExprCapable> = Const<C, T>;
}

impl<C: ExprCapable> Functor for Const<C, ()> {
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        Expression::new(FnType::new(|_| {
            FnType::new(|c| {
                let Const { data, .. } = DataExpr::destructure(c);
                Const {
                    data,
                    _ph: PhantomData,
                }
            })
        }))
    }
}

impl<CO: Monoid> Applicative for Const<CO, ()> {
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        constant().apply(Expression::new(Const {
            data: CO::empty(),
            _ph: PhantomData,
        }))
    }

    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        constant().apply(Expression::new(FnType::new(|c1| {
            FnType::new(|c2| {
                let (Const { data: c1, .. }, Const { data: c2, .. }) =
                    (DataExpr::destructure(c1), DataExpr::destructure(c2));
                Const {
                    data: CO::append().apply(c1).apply(c2),
                    _ph: PhantomData,
                }
            })
        })))
    }
}
