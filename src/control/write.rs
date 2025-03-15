use std::marker::PhantomData;

use crate::{
    data::{
        pair::{map_snd, pair, map2_pair},
        Monoid, Type,
    },
    expression::{DataExpr, ExprCapable, Expression, FnType},
    Expr, Tup,
};

use super::{Applicative, Functor, Monad, TypeCtor};

/// The writer monad.
#[derive(Debug, Clone)]
pub struct Write<C: Type>(PhantomData<C>);

impl<C: Type> ExprCapable for Write<C> {}
impl<C: Type> TypeCtor for Write<C> {
    type Apply<A: ExprCapable> = Tup!(C::Apply, A);
}

impl<C: Type> Functor for Write<C> {
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        map_snd()
    }
}

impl<CO: Monoid> Applicative for Write<CO> {
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        pair().apply(CO::empty())
    }

    // map2 f (s, a) (s', b) = (s <> s', f a b)
    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        map2_pair().apply(CO::append())
    }
}

impl<C: Monoid> Monad for Write<C> {
    // bind f (s, a) = let (s', b) = f a in (s <> s', b)
    // bind f msa = do (s, a) <- msa; (s', b) <- f a; pure (s <> s', b)
    fn bind<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((A => Self::Apply<B>) => Self::Apply<A> => Self::Apply<B>) {
        Expression::new(FnType::new(|f| {
            FnType::new(|pair| {
                let (s1, a) = DataExpr::destructure(pair);
                let (s2, b) = DataExpr::destructure(f.apply(a));
                (C::append().apply(s1).apply(s2), b)
            })
        }))
    }
}
