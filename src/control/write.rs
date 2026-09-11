use std::marker::PhantomData;

use crate::{
    Expr, Tup,
    data::{
        Monoid, Type,
        pair::{map_snd, map2_pair, pair},
    },
    expression::{ExprCapable, Expression, FnType},
    funexp, mdo,
};

use super::{Applicative, Functor, Monad, TypeCtor, identity::Identity};

/// The writer monad.
#[derive(Debug, Clone)]
pub struct WriteT<C: Type, T: TypeCtor>(PhantomData<(C, T)>);

pub type Write<C> = WriteT<C, Identity>;

impl<C: Type, T: TypeCtor> ExprCapable for WriteT<C, T> {}
impl<C: Type, T: TypeCtor> TypeCtor for WriteT<C, T> {
    type Apply<A: ExprCapable> = T::Apply<Tup!(C::Apply, A)>;
}

impl<C: Type, T: Functor> Functor for WriteT<C, T> {
    // map f = map (map_snd f)
    fn map<A: ExprCapable, B: ExprCapable>()
    -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        T::map().compose(map_snd())
    }
}

impl<CO: Monoid, T: Applicative> Applicative for WriteT<CO, T> {
    // pure a = pure (empty, a)
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        T::pure().compose(pair().apply(CO::empty()))
    }

    // map2 f = map2 (map2_pair append f)
    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        T::map2().compose(map2_pair().apply(CO::append()))
    }
}

impl<C: Monoid, T: Monad> Monad for WriteT<C, T> {
    // bind f (s, a) = let (s', b) = f a in (s <> s', b)
    // bind f msa = do (s, a) <- msa; (s', b) <- f a; pure (s <> s', b)
    fn bind<A: ExprCapable, B: ExprCapable>()
    -> Expr!((A => Self::Apply<B>) => Self::Apply<A> => Self::Apply<B>) {
        funexp!(|f, ma| mdo!({
            use T;
            let pat!((c1, a)) = ma;
            let pat!((c2, b)) = f.apply(a);
            T::pure().apply_value((C::append().apply(c1).apply(c2), b))
        })
        .eval())
    }
}
