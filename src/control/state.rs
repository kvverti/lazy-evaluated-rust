use std::marker::PhantomData;

use crate::{
    data::{
        pair::{map_snd, Pair},
        Type,
    },
    expression::{ExprCapable, Expression, FnType},
    function::compose,
    ExprType,
};

use super::{identity::Identity, Functor, TypeCtor};

pub type StateFn<S, T, A> = ExprType!(S => type <T as TypeCtor>::Apply<Pair<S, A>>);

#[derive(Debug, Clone)]
pub struct StateT<S: Type, T: TypeCtor>(PhantomData<(S, T)>);
pub type State<S> = StateT<S, Identity>;

impl<S: Type, T: TypeCtor> ExprCapable for StateT<S, T> {}

impl<S: Type, T: TypeCtor> TypeCtor for StateT<S, T> {
    type Apply<A: ExprCapable> = StateFn<S::Apply, T, A>;
}

impl<S: Type, T: Functor> Functor for StateT<S, T> {
    // map f st s = map (map_snd f) (st s)
    // map f = (.) (map (map_snd f))
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        compose().compose(T::map()).compose(map_snd())
    }
}
