use std::marker::PhantomData;

use crate::{
    Expr, ExprType,
    control::{Applicative, Monad, MonadFix},
    data::{
        Type,
        pair::{Pair, map_snd},
    },
    expression::{ExprCapable, Expression, FnType},
    fun,
    function::compose,
    funexp, mdo,
};

use super::{Functor, TypeCtor, identity::Identity};

pub type StateFn<S, T, A> = ExprType!((type <S as Type>::Apply) => type <T as TypeCtor>::Apply<Pair<<S as Type>::Apply, A>>);

#[derive(Debug, Clone)]
pub struct StateT<S: Type, T: TypeCtor>(PhantomData<(S, T)>);
pub type State<S> = StateT<S, Identity>;

impl<S: Type, T: TypeCtor> ExprCapable for StateT<S, T> {}

impl<S: Type, T: TypeCtor> TypeCtor for StateT<S, T> {
    type Apply<A: ExprCapable> = StateFn<S, T, A>;
}

impl<S: Type, T: Functor> Functor for StateT<S, T> {
    // map f st s = map (map_snd f) (st s)
    // map f = (.) (map (map_snd f))
    fn map<A: ExprCapable, B: ExprCapable>()
    -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        compose().compose(T::map()).compose(map_snd())
    }
}

impl<S: Type, T: Monad> Applicative for StateT<S, T> {
    // pure a s = T::pure (s, a)
    fn pure<A: ExprCapable>() -> Expr!(A => Self::Apply<A>) {
        funexp!(|a, s| T::pure().apply_value((s, a)).eval())
    }

    // map2 f fsa fsb s = T::do (
    //                      let (s1, a) = fsa s
    //                          (s2, b) = fsb s1
    //                      pure (s2, f a b)
    //                    )
    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>()
    -> Expr!((A => B => C) => Self::Apply<A> => Self::Apply<B> => Self::Apply<C>) {
        funexp!(|f, sta, stb, s| mdo!({
            use T;
            let pat!((s1, a)) = sta.apply(s);
            let pat!((s2, b)) = stb.apply(s1);
            T::pure().apply_value((s2, f.apply(a).apply(b)))
        })
        .eval())
    }
}

impl<S: Type, T: Monad> Monad for StateT<S, T> {
    // bind f fsa s = T::do (s1, a) = fsa s
    //                   f a s1
    fn bind<A: ExprCapable, B: ExprCapable>()
    -> Expr!((A => Self::Apply<B>) => Self::Apply<A> => Self::Apply<B>) {
        funexp!(|f, sta, s| mdo!({
            use T;
            let pat!((s, a)) = sta.apply(s);
            f.apply(a).apply(s)
        })
        .eval())
    }
}

// mfix :: (a -> s -> (s, a)) -> s -> (s, a)
// mfix f = fix (\st s -> let (s', a) = st s; f a s')
impl<S: Type, T: Monad> MonadFix for StateT<S, T> {
    fn mfix<A: ExprCapable>() -> Expr!((A => Self::Apply<A>) => Self::Apply<A>) {
        funexp!(|f| Expression::fix(fun!(|st, s| mdo!({
            use T;
            let pat!((s, a)) = st.apply(s);
            f.apply(a).apply(s)
        })
        .eval()))
        .eval())
    }
}
