use std::marker::PhantomData;

use crate::{expression::{ExprCapable, Expression, FnType}, data::Type};

use super::{TypeCtor, identity::Identity, Functor};

pub type StateT<S: ExprCapable, T: TypeCtor, A: ExprCapable> = FnType<S, T::Apply<(Expression<S>, Expression<A>)>>;

pub type State<S: ExprCapable, A: ExprCapable> = StateT<S, Identity, A>;

#[derive(Debug, Clone)]
pub struct Canonical<S: Type, T: TypeCtor>(PhantomData<(S, T)>);

impl<S: Type, T: TypeCtor> ExprCapable for Canonical<S, T> {}

impl<S: Type, T: TypeCtor> TypeCtor for Canonical<S, T> {
    type Apply<A: ExprCapable> = StateT<S::Apply, T, A>;
}

impl<S: Type, T: Functor> Functor for Canonical<S, T> {
    // map f st s = map (\(s', a) -> (s', f a)) (st s)
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        todo!()
    }
}