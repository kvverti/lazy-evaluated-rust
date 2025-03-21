use std::marker::PhantomData;

use crate::{
    control::{Alt, Applicative, Functor, Monad, MonadFix, Traversable, TypeCtor},
    expression::{DataExpr, ExprCapable, Expression, FnType},
    function::{constant, id},
    undefined, Expr,
};

use super::{compose::Compose, Associative, Foldable, Monoid, Type};

#[derive(Debug, Clone)]
pub enum Maybe<T> {
    Nothing,
    Just(Expression<T>),
}

impl<T: ExprCapable> ExprCapable for Maybe<T> {}
impl<T: ExprCapable> DataExpr for Maybe<T> {
    fn destructure(v: Expression<Self>) -> Self {
        v.eval()
    }
}

#[derive(Debug, Clone)]
pub struct Monoidal<T: Type>(PhantomData<T>);

impl<T: Type> ExprCapable for Monoidal<T> {}

impl<T: Type> Type for Monoidal<T> {
    type Apply = Maybe<T::Apply>;
}

impl<T: Associative> Associative for Monoidal<T> {
    fn append() -> Expression<FnType<Self::Apply, FnType<Self::Apply, Self::Apply>>> {
        Expression::new(FnType::new(|maybe_a| {
            FnType::new(|maybe_b| {
                match (
                    DataExpr::destructure(maybe_a),
                    DataExpr::destructure(maybe_b),
                ) {
                    (Maybe::Just(a), Maybe::Just(b)) => Maybe::Just(T::append().apply(a).apply(b)),
                    _ => Maybe::Nothing,
                }
            })
        }))
    }
}

impl<T: Associative> Monoid for Monoidal<T> {
    fn empty() -> Expression<Self::Apply> {
        Expression::new(Maybe::Nothing)
    }
}

impl TypeCtor for Maybe<()> {
    type Apply<T: ExprCapable> = Maybe<T>;
}

impl Alt for Maybe<()> {
    fn none<A: ExprCapable>() -> Expr!(Self::Apply<A>) {
        Expression::new(Maybe::Nothing)
    }

    // alt (Just a) _ = Just a
    // alt Nothing x = x
    fn alt<A: ExprCapable>() -> Expr!(Self::Apply<A> => Self::Apply<A> => Self::Apply<A>) {
        Expression::new(FnType::new(|maybe| match DataExpr::destructure(maybe) {
            Maybe::Nothing => id().eval(),
            Maybe::Just(x) => constant().apply(Expression::new(Maybe::Just(x))).eval(),
        }))
    }
}

impl Foldable for Maybe<()> {
    fn foldr<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, FnType<B, B>>, FnType<B, FnType<Self::Apply<A>, B>>>> {
        Expression::new(FnType::new(|f| {
            FnType::new(|b| {
                FnType::new(|maybe_a| match DataExpr::destructure(maybe_a) {
                    Maybe::Nothing => b.eval(),
                    Maybe::Just(a) => f.apply(a).apply(b).eval(),
                })
            })
        }))
    }
}

impl Functor for Maybe<()> {
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        Expression::new(FnType::new(|f| {
            FnType::new(|maybe_a| match DataExpr::destructure(maybe_a) {
                Maybe::Nothing => Maybe::Nothing,
                Maybe::Just(a) => Maybe::Just(f.apply(a)),
            })
        }))
    }
}

impl Applicative for Maybe<()> {
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        Expression::new(FnType::new(Maybe::Just))
    }

    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        Expression::new(FnType::new(|f| {
            FnType::new(|maybe_a| {
                FnType::new(|maybe_b| {
                    match (
                        DataExpr::destructure(maybe_a),
                        DataExpr::destructure(maybe_b),
                    ) {
                        (Maybe::Just(a), Maybe::Just(b)) => Maybe::Just(f.apply(a).apply(b)),
                        _ => Maybe::Nothing,
                    }
                })
            })
        }))
    }
}

impl Monad for Maybe<()> {
    fn bind<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, Self::Apply<B>>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        Expression::new(FnType::new(|f| {
            FnType::new(|maybe_a| match DataExpr::destructure(maybe_a) {
                Maybe::Nothing => Maybe::Nothing,
                Maybe::Just(a) => f.apply(a).eval(),
            })
        }))
    }
}

impl MonadFix for Maybe<()> {
    // mfix (a -> (Just a)) = Just a
    // mfix (_ -> Nothing) = Nothing
    fn mfix<A: ExprCapable>() -> Expr!((A => Self::Apply<A>) => Self::Apply<A>) {
        Expression::new(FnType::new(|f| {
            Expression::fix(FnType::new(|maybe| {
                f.apply(Expression::lazy(|| match DataExpr::destructure(maybe) {
                    Maybe::Just(x) => x.eval(),
                    Maybe::Nothing => undefined().eval(),
                }))
                .eval()
            }))
            .eval()
        }))
    }
}

impl Traversable for Maybe<()> {
    fn traverse<F: Applicative, A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, F::Apply<B>>, FnType<Self::Apply<A>, F::Apply<Self::Apply<B>>>>>
    {
        Expression::new(FnType::new(|f| {
            FnType::new(|maybe_a| match DataExpr::destructure(maybe_a) {
                Maybe::Nothing => F::pure().apply(Expression::new(Maybe::Nothing)).eval(),
                Maybe::Just(a) => F::map().apply(Self::pure()).apply(f.apply(a)).eval(),
            })
        }))
    }
}

/// The Maybe monad transformer, which adds optionality to any given monad.
pub type MaybeT<M, A> = <Compose<M, Maybe<()>> as TypeCtor>::Apply<A>;

impl<M: Monad> Monad for Compose<M, Maybe<()>> {
    fn bind<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, Self::Apply<B>>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        Expression::new(FnType::new(|f| {
            FnType::new(|ma: Expression<MaybeT<M, A>>| {
                M::bind()
                    .apply(Expression::new(FnType::new(
                        |maybe_a: Expression<Maybe<A>>| match DataExpr::destructure(maybe_a) {
                            Maybe::Nothing => {
                                M::pure().apply(Expression::new(Maybe::Nothing)).eval()
                            }
                            Maybe::Just(a) => f.apply(a).eval(),
                        },
                    )))
                    .apply(ma)
                    .eval()
            })
        }))
    }
}
