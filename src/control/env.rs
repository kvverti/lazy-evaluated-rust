use std::marker::PhantomData;

use crate::{
    data::Type,
    expression::{ExprCapable, Expression, FnType},
    function::{combine, compose, constant},
};

use super::{identity::Identity, Applicative, Functor, Monad, TypeCtor};

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
    fn bind<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, Self::Apply<B>>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        Expression::new(FnType::new(|f| {
            FnType::new(|fenv| {
                FnType::new(|env| {
                    let e = env.clone();
                    T::bind()
                        .apply(Expression::new(FnType::new(|a| {
                            f.apply(a).apply(env).eval()
                        })))
                        .apply(fenv.apply(e))
                        .eval()
                })
            })
        }))
    }
}
