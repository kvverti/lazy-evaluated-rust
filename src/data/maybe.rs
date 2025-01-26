use crate::{
    control::{Applicative, Functor, Monad, Traversable, TypeCtor},
    expression::{DataExpr, ExprCapable, Expression, FnType},
};

use super::{compose::Compose, Associative, Foldable, Monoid, Newtype};

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

impl<T: Associative> Associative for Maybe<T> {
    fn append() -> Expression<FnType<Self, FnType<Self, Self>>> {
        Expression::new(FnType::new(|maybe_a| {
            FnType::new(|maybe_b| {
                match (
                    DataExpr::destructure(maybe_a),
                    DataExpr::destructure(maybe_b),
                ) {
                    (Self::Just(a), Self::Just(b)) => Self::Just(T::append().apply(a).apply(b)),
                    _ => Self::Nothing,
                }
            })
        }))
    }
}

impl<T: Associative> Monoid for Maybe<T> {
    fn empty() -> Expression<Self> {
        Expression::new(Self::Nothing)
    }
}

impl TypeCtor for Maybe<()> {
    type Apply<T: ExprCapable> = Maybe<T>;
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
pub type MaybeT<M, A> = Compose<M, Maybe<()>, A>;

impl<M: Monad> Monad for MaybeT<M, ()> {
    fn bind<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, Self::Apply<B>>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        Expression::new(FnType::new(|f| {
            FnType::new(|ma: Expression<MaybeT<M, A>>| {
                let ma = FnType::new(Newtype::unlift).apply(ma);
                MaybeT::lift(
                    M::bind()
                        .apply(Expression::new(FnType::new(
                            |maybe_a: Expression<Maybe<A>>| match DataExpr::destructure(maybe_a) {
                                Maybe::Nothing => {
                                    M::pure().apply(Expression::new(Maybe::Nothing)).eval()
                                }
                                Maybe::Just(a) => MaybeT::unlift(f.apply(a)),
                            },
                        )))
                        .apply(ma),
                )
            })
        }))
    }
}
