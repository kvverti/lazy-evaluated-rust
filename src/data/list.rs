use crate::{
    control::{Applicative, Functor, Monad, Traversable, TypeCtor},
    expression::{DataExpr, ExprCapable, Expression, FnType},
    function::{combine, compose, constant},
    Expr,
};

use super::{Associative, Foldable, Monoid, Type};

/// A lazy cons list.
#[derive(Debug, Clone)]
pub enum ConsList<T> {
    Nil,
    Cons {
        head: Expression<T>,
        tail: Expression<Self>,
    },
}

impl<T: ExprCapable> ConsList<T> {
    pub fn concat() -> Expr!(Self => Self => Self) {
        Expression::fix(crate::fun!(|rec, list_a, list_b| match DataExpr::destructure(list_a) {
            ConsList::Nil => list_b.eval(),
            ConsList::Cons { head, tail } => ConsList::Cons {
                head,
                tail: rec.apply(tail).apply(list_b),
            },
        }))
    }
}

impl<T: ExprCapable> ExprCapable for ConsList<T> {}

impl<T: ExprCapable> DataExpr for ConsList<T> {
    fn destructure(v: Expression<Self>) -> Self {
        v.eval()
    }
}

impl<T: ExprCapable> Type for ConsList<T> {
    type Apply = Self;
}

impl<T: ExprCapable> Associative for ConsList<T> {
    fn append() -> Expression<FnType<Self, FnType<Self, Self>>> {
        Self::concat()
    }
}

impl<T: ExprCapable> Monoid for ConsList<T> {
    fn empty() -> Expression<Self> {
        Expression::new(Self::Nil)
    }
}

impl TypeCtor for ConsList<()> {
    type Apply<T: ExprCapable> = ConsList<T>;
}

impl Foldable for ConsList<()> {
    fn foldr<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, FnType<B, B>>, FnType<B, FnType<Self::Apply<A>, B>>>> {
        Expression::new(FnType::new(|f| {
            FnType::new(|b| {
                Expression::fix(FnType::new(|rec| {
                    FnType::new(|ls| match DataExpr::destructure(ls) {
                        ConsList::Nil => b.eval(),
                        ConsList::Cons { head, tail } => {
                            f.apply(head).apply(rec.apply(tail)).eval()
                        }
                    })
                }))
                .eval()
            })
        }))
    }

    fn foldl_strict<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<B, FnType<A, B>>, FnType<B, FnType<Self::Apply<A>, B>>>> {
        Expression::new(FnType::new(|f| {
            Expression::fix(FnType::new(|rec| {
                FnType::new(|b| {
                    FnType::new(|ls| match DataExpr::destructure(ls) {
                        ConsList::Nil => b.eval(),
                        ConsList::Cons { head, tail } => {
                            rec.apply_strict(f.apply(b).apply(head)).apply(tail).eval()
                        }
                    })
                })
            }))
            .eval()
        }))
    }
}

impl Functor for ConsList<()> {
    // map f = foldr (\a -> (:) (f a)) []
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        Expression::new(FnType::new(|f| {
            Self::foldr()
                .apply(Expression::new(FnType::new(|a| {
                    FnType::new(|bs| ConsList::Cons {
                        head: f.apply(a),
                        tail: bs,
                    })
                })))
                .apply(ConsList::empty())
                .eval()
        }))
    }
}

impl Traversable for ConsList<()> {
    // traverse f [] = pure []
    // traverse f (a:as) = map2 (:) (f a) (traverse f as)
    fn traverse<F: Applicative, A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, F::Apply<B>>, FnType<Self::Apply<A>, F::Apply<Self::Apply<B>>>>>
    {
        Expression::new(FnType::new(|f| {
            Expression::fix(FnType::new(|rec| {
                FnType::new(|ls| match DataExpr::destructure(ls) {
                    ConsList::Nil => F::pure().apply(ConsList::empty()).eval(),
                    ConsList::Cons { head, tail } => F::map2()
                        .apply(Expression::new(FnType::new(|x| {
                            FnType::new(|xs| ConsList::Cons { head: x, tail: xs })
                        })))
                        .apply(f.apply(head))
                        .apply(rec.apply(tail))
                        .eval(),
                })
            }))
            .eval()
        }))
    }
}

#[derive(Debug, Clone)]
pub struct Cartesian;

impl ExprCapable for Cartesian {}

impl TypeCtor for Cartesian {
    type Apply<T: ExprCapable> = ConsList<T>;
}

impl Functor for Cartesian {
    fn map<A: ExprCapable, B: ExprCapable>() -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>) {
        ConsList::map()
    }
}

impl Applicative for Cartesian {
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        Expression::new(FnType::new(|a| ConsList::Cons {
            head: a,
            tail: Expression::new(ConsList::Nil),
        }))
    }

    // map2 f [] _ = []
    // map2 f (a:as) = combine (++) (map $ f a) (map2 f as)
    // where combine f g h = \x -> f (g x) (h x)
    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        Expression::new(FnType::new(|f| {
            Expression::fix(FnType::new(|rec| {
                FnType::new(|list_a| match DataExpr::destructure(list_a) {
                    ConsList::Nil => constant().apply(Expression::new(ConsList::Nil)).eval(),
                    ConsList::Cons { head, tail } => combine()
                        .apply(ConsList::concat())
                        .apply(Self::map().apply(f.apply(head)))
                        .apply(rec.apply(tail))
                        .eval(),
                })
            }))
            .eval()
        }))
    }
}

impl Monad for Cartesian {
    // bind f = join . (map f)
    fn bind<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, Self::Apply<B>>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        Expression::new(FnType::new(|f| {
            compose()
                .apply(Self::join())
                .apply(Self::map().apply(f))
                .eval()
        }))
    }

    // join ls = foldr concat [] ls
    fn join<A: ExprCapable>() -> Expression<FnType<Self::Apply<Self::Apply<A>>, Self::Apply<A>>> {
        ConsList::foldr()
            .apply(ConsList::concat())
            .apply(ConsList::empty())
    }
}

#[derive(Debug, Clone)]
pub struct Pairwise;

impl ExprCapable for Pairwise {}

impl TypeCtor for Pairwise {
    type Apply<T: ExprCapable> = ConsList<T>;
}

impl Functor for Pairwise {
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        ConsList::map()
    }
}

impl Applicative for Pairwise {
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        crate::repeat()
    }

    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        Expression::new(FnType::new(|f| {
            Expression::fix(FnType::new(|rec| {
                FnType::new(|list_a| {
                    FnType::new(|list_b| {
                        match (DataExpr::destructure(list_a), DataExpr::destructure(list_b)) {
                            (
                                ConsList::Cons { head: a, tail: axs },
                                ConsList::Cons { head: b, tail: bxs },
                            ) => ConsList::Cons {
                                head: f.apply(a).apply(b),
                                tail: rec.apply(axs).apply(bxs),
                            },
                            _ => ConsList::Nil,
                        }
                    })
                })
            }))
            .eval()
        }))
    }
}
