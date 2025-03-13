use crate::{
    expression::{ExprCapable, Expression, FnType},
    function::{constant, id},
};

use super::{Applicative, Functor, Monad, TypeCtor};

/// The identity monad.
#[derive(Debug, Clone)]
pub struct Identity;

impl ExprCapable for Identity {}
impl TypeCtor for Identity {
    type Apply<T: ExprCapable> = T;
}

impl Functor for Identity {
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        id()
    }
}

impl Applicative for Identity {
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        id()
    }

    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        id()
    }

    fn ap<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<Self::Apply<FnType<A, B>>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        id()
    }
}

impl Monad for Identity {
    fn bind<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, Self::Apply<B>>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        id()
    }

    fn join<A: ExprCapable>() -> Expression<FnType<Self::Apply<Self::Apply<A>>, Self::Apply<A>>> {
        id()
    }

    fn sequence<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<Self::Apply<B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        constant()
    }
}
