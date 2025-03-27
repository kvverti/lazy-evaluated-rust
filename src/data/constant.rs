use std::marker::PhantomData;

use crate::{
    control::{Alt, Applicative, Functor, Traversable, TypeCtor},
    expression::{ExprCapable, Expression, FnType},
    function::{constant, id},
    Expr, ExprType,
};

use super::{Foldable, Monoid, Type};

/// The const functor. This functor maps all operations to the identity function of the type `C`.
/// If `C` possesses a monoid, the const functor maps application to the associative operation.
#[derive(Debug, Clone)]
pub struct Const<C: Type>(PhantomData<C>);

impl<C: Type> ExprCapable for Const<C> {}

impl<C: Type> TypeCtor for Const<C> {
    type Apply<T: ExprCapable> = C::Apply;
}

impl<C: Type> Functor for Const<C> {
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        constant().apply(id())
    }
}

impl<CO: Monoid> Applicative for Const<CO> {
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        constant().apply(CO::empty())
    }

    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        constant().apply(CO::append())
    }

    fn ap<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!(Self::Apply<ExprType!(A => B)> => Self::Apply<A> => Self::Apply<B>) {
        CO::append()
    }
}

impl<C: Monoid> Alt for Const<C> {
    fn none<A: ExprCapable>() -> Expr!(Self::Apply<A>) {
        C::empty()
    }

    fn alt<A: ExprCapable>() -> Expr!(Self::Apply<A> => Self::Apply<A> => Self::Apply<A>) {
        C::append()
    }
}

impl<C: Type> Foldable for Const<C> {
    // foldr _ b _ = b
    fn foldr<A: ExprCapable, B: ExprCapable>() -> Expr!((A => B => B) => B => Self::Apply<A> => B) {
        constant().apply(constant())
    }

    fn foldl_strict<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((B => A => B) => B => Self::Apply<A> => B) {
        constant().apply(constant())
    }

    // fold_map = const (const empty)
    fn fold_map<M: Monoid, A: ExprCapable>() -> Expr!((A => M::Apply) => Self::Apply<A> => M::Apply)
    {
        constant().apply(constant().apply(M::empty()))
    }

    fn fold<M: Monoid, A: ExprCapable>() -> Expr!(Self::Apply<M::Apply> => M::Apply) {
        constant().apply(M::empty())
    }
}

impl<C: Type> Traversable for Const<C> {
    // traverse _ = pure
    fn traverse<F: Applicative, A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((A => F::Apply<B>) => Self::Apply<A> => F::Apply<Self::Apply<B>>) {
        constant().apply(F::pure())
    }

    // sequence = pure
    fn sequence<F: Applicative, A: ExprCapable>(
    ) -> Expr!(Self::Apply<F::Apply<A>> => F::Apply<Self::Apply<A>>) {
        F::pure()
    }
}
