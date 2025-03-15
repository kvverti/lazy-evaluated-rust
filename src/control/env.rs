use std::marker::PhantomData;

use crate::{
    data::{Monoid, Type},
    expression::{ExprCapable, Expression, FnType},
    function::{call, combine, compose, constant, flip, s},
    Expr,
};

use super::{identity::Identity, Applicative, Comonad, Functor, Monad, TypeCtor};

pub type Env<R> = EnvT<R, Identity>;

/// The environment (reader) monad.
#[derive(Debug, Clone)]
pub struct EnvT<R: Type, T: TypeCtor>(PhantomData<(R, T)>);

impl<R: Type, T: TypeCtor> ExprCapable for EnvT<R, T> {}
impl<R: Type, T: TypeCtor> TypeCtor for EnvT<R, T> {
    type Apply<A: ExprCapable> = FnType<R::Apply, T::Apply<A>>;
}

impl<R: Type, T: Functor> Functor for EnvT<R, T> {
    // map f fenv env = map f (fenv env)
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        compose().compose(T::map())
    }
}

impl<R: Type, T: Applicative> Applicative for EnvT<R, T> {
    // pure a _ = pure a
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        constant().compose(T::pure())
    }

    // map2 f fe1 fe2 env = map2 f (fe1 env) (fe2 env)
    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        combine().compose(T::map2())
    }
}

impl<R: Type, T: Monad> Monad for EnvT<R, T> {
    // bind :: (a -> r -> t b) -> (r -> t a) -> r -> t b
    // bind f fenv env = do a <- fenv env; f a env
    // bind f fenv env = bind (\a -> f a env) (fenv env)
    // bind f fenv env = bind (flip f env) (fenv env)
    // bind f fenv env = (bind . flip f) env (fenv env)
    // bind = combine bind . flip
    fn bind<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, Self::Apply<B>>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        combine().apply(T::bind()).compose(flip())
    }

    // join :: (r -> t (r -> t a)) -> r -> t a
    // join f r = bind ((&) r) (f r)
    // join = combine bind (&)
    fn join<A: ExprCapable>() -> Expr!(Self::Apply<Self::Apply<A>> => Self::Apply<A>) {
        s().apply(T::bind().compose(call()))
    }

    // sequence :: (r -> t b) -> (r -> t a) -> (r -> t b)
    // sequence fb fa r = do _ <- fa r; fb r
    // sequence fb fa r = sequence (fb r) (fa r)
    fn sequence<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!(Self::Apply<B> => Self::Apply<A> => Self::Apply<B>) {
        combine().apply(T::sequence())
    }
}

impl<R: Monoid> Comonad for Env<R> {
    fn extract<A: ExprCapable>() -> Expr!(Self::Apply<A> => A) {
        call().apply(R::empty())
    }

    fn extend<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((Self::Apply<A> => B) => Self::Apply<A> => Self::Apply<B>) {
        Expression::new(FnType::new(|f| {
            Self::map().apply(f).compose(Self::duplicate()).eval()
        }))
    }

    fn duplicate<A: ExprCapable>() -> Expr!(Self::Apply<A> => Self::Apply<Self::Apply<A>>) {
        Expression::new(FnType::new(|f| {
            FnType::new(|r1| FnType::new(|r2| f.apply(R::append().apply(r1).apply(r2)).eval()))
        }))
    }
}
