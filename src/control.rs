use crate::{expression::{ExprCapable, Expression, FnType}, data::Foldable};

pub mod state;

pub trait TypeCtor: ExprCapable {
    type Apply<T: ExprCapable>: ExprCapable;
}

pub trait Functor: TypeCtor {
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>>;
}

pub trait Applicative: Functor {
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>>;

    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    >;

    fn ap<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<Self::Apply<FnType<A, B>>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        Self::map2().apply(super::id())
    }
}

pub trait Monad: Applicative {
    fn bind<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, Self::Apply<B>>, FnType<Self::Apply<A>, Self::Apply<B>>>>;

    fn join<A: ExprCapable>() -> Expression<FnType<Self::Apply<Self::Apply<A>>, Self::Apply<A>>> {
        Self::bind().apply(super::id())
    }

    fn sequence<A: ExprCapable, B: ExprCapable>() -> Expression<FnType<Self::Apply<B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        super::compose().apply(Self::bind()).apply(super::constant())
    }
}

pub trait Traversable: Functor + Foldable {
    fn traverse<F: Applicative, A: ExprCapable, B: ExprCapable>() -> Expression<FnType<FnType<A, F::Apply<B>>, FnType<Self::Apply<A>, F::Apply<Self::Apply<B>>>>>;

    fn sequence<F: Applicative, A: ExprCapable>() -> Expression<FnType<Self::Apply<F::Apply<A>>, F::Apply<Self::Apply<A>>>> {
        Self::traverse::<F, _, _>().apply(super::id())
    }
}
