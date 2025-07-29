use crate::{
    control::{identity::Identity, Alt, Functor, TypeCtor},
    expression::{DataExpr, ExprCapable, Expression, FnType},
    fun,
    function::{combine, flip},
    letrec, Expr,
};

use super::maybe::Maybe;

/// An unbounded sequence, parameterized by some type constructor `T`.
/// I have not verified that the trait implementations for this type are correct.
pub type StreamT<T, A> = <T as TypeCtor>::Apply<Cons<T, A>>;

pub type Stream<A> = StreamT<Identity, A>;

pub type List<A> = StreamT<Maybe<()>, A>;

/// An unbounded sequence whose tail is parameterized by some type constructor `T`.
pub type NonEmptyStreamT<T, A> = Cons<T, A>;

/// A sequence of one or more elements.
pub type NonEmptyList<A> = NonEmptyStreamT<Maybe<()>, A>;

#[derive(Clone)]
pub struct Cons<T: TypeCtor, A: ExprCapable> {
    head: Expression<A>,
    tail: Expression<StreamT<T, A>>,
}

impl<T: TypeCtor, A: ExprCapable> ExprCapable for Cons<T, A> {}
impl<T: TypeCtor, A: ExprCapable> DataExpr for Cons<T, A> {
    fn destructure(v: Expression<Self>) -> Self {
        let v1 = v.clone();
        Self {
            head: Expression::lazy(|| v.eval().head.eval()),
            tail: Expression::lazy(|| v1.eval().tail.eval()),
        }
    }
}

impl<T: TypeCtor, A: ExprCapable> Cons<T, A> {
    pub fn new() -> Expr!(A => StreamT<T, A> => Self) {
        Expression::new(fun!(|head, tail| Self { head, tail }))
    }

    pub fn head() -> Expr!(Self => A) {
        Expression::new(FnType::new(|cons| {
            let Cons { head, tail: _ } = DataExpr::destructure(cons);
            head.eval()
        }))
    }

    pub fn tail() -> Expr!(Self => StreamT<T, A>) {
        Expression::new(FnType::new(|cons| {
            let Cons { head: _, tail } = DataExpr::destructure(cons);
            tail.eval()
        }))
    }
}

// repeat ta = map (\a -> Cons a (repeat ta)) ta
// repeat ta = fix (\xs -> map (\a -> Cons a xs) ta)
pub fn repeat<T: Functor, A: ExprCapable>() -> Expr!(T::Apply<A> => StreamT<T, A>) {
    Expression::new(FnType::new(|elem| {
        Expression::fix(FnType::new(|stream| {
            T::map()
                .apply(flip().apply(Cons::new()).apply(stream))
                .apply(elem)
                .eval()
        }))
        .eval()
    }))
}

// flipconcat tys = rec0
//  where rec1 (x:txs') = Cons::new x (rec0 txs')
//        rec0 = flip alt tys . map rec1
pub fn concat<T: Functor + Alt, A: ExprCapable>(
) -> Expr!(StreamT<T, A> => StreamT<T, A> => StreamT<T, A>) {
    flip().apply(Expression::new(FnType::new(|tys| {
        letrec! {
            let rec0 = flip().apply(T::alt()).apply(tys).compose(T::map().apply(rec1));
            let rec1 = combine().apply(Cons::new()).apply(Cons::head()).apply(rec0.compose(Cons::tail()));
            rec0.eval()
        }
    })))
}

pub mod instance {
    use std::marker::PhantomData;

    use crate::{
        control::{identity::Identity, Alt, Applicative, Functor, Monad, Traversable, TypeCtor},
        data::Foldable,
        expression::{DataExpr, ExprCapable, Expression, FnType},
        fun,
        function::{call, combine, flip},
        letrec, Expr, ExprType,
    };

    use super::{concat, repeat, Cons};

    #[derive(Debug, Clone)]
    pub struct StreamT<T: TypeCtor>(PhantomData<T>);

    pub type Stream = StreamT<Identity>;

    impl<T: TypeCtor> ExprCapable for StreamT<T> {}
    impl<T: TypeCtor> TypeCtor for StreamT<T> {
        type Apply<A: ExprCapable> = super::StreamT<T, A>;
    }

    impl<T: Functor> Functor for StreamT<T> {
        // map f = map (\(x:xs) -> f x : map f xs)
        // map f = map (combine new (f . head) (map f . tail))
        fn map<A: ExprCapable, B: ExprCapable>(
        ) -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>) {
            Expression::new(FnType::new(|f| {
                Expression::fix(FnType::new(|rec| {
                    T::map()
                        .apply(
                            combine()
                                .apply(Cons::new())
                                .apply(f.compose(Cons::head()))
                                .apply(rec.compose(Cons::tail())),
                        )
                        .eval()
                }))
                .eval()
            }))
        }
    }

    impl<T: Foldable> Foldable for StreamT<T> {
        // foldr f = T::foldr (\(x:txs) b -> (f x (Self::foldr f b txs)))
        // foldr f = rec0
        //     where rec0 = T::foldr rec
        //           rec (x:txs) b = f x (rec0 b txs)
        fn foldr<A: ExprCapable, B: ExprCapable>(
        ) -> Expr!((A => B => B) => B => Self::Apply<A> => B) {
            Expression::new(FnType::new(|f| {
                letrec! {
                    let rec0 = T::foldr().apply(rec1);
                    let rec1 = Expression::new(fun!(|cons, b| {
                        let Cons { head, tail } = DataExpr::destructure(cons);
                        f.apply(head).apply(rec0.apply(b).apply(tail)).eval()
                    }));
                    rec0.eval()
                }
            }))
        }
    }

    impl<T: Traversable> Traversable for StreamT<T> {
        // traverse f = rec0
        //  where rec0 = T::traverse rec1
        //        rec1 (x:txs) = F::map2 Cons::new (f x) (rec0 txs)
        fn traverse<F: Applicative, A: ExprCapable, B: ExprCapable>(
        ) -> Expr!((A => F::Apply<B>) => Self::Apply<A> => F::Apply<Self::Apply<B>>) {
            Expression::new(FnType::new(|f| {
                letrec! {
                    let rec0 = T::traverse::<F, _, _>().apply(rec1);
                    let rec1 = combine()
                        .apply(F::map2().apply(Cons::new()))
                        .apply(f.compose(Cons::head()))
                        .apply(rec0.compose(Cons::tail()));
                    rec0.eval()
                }
            }))
        }
    }

    #[derive(Debug, Clone)]
    pub struct Pairwise<T: TypeCtor>(PhantomData<T>);

    impl<T: TypeCtor> ExprCapable for Pairwise<T> {}
    impl<T: TypeCtor> TypeCtor for Pairwise<T> {
        type Apply<A: ExprCapable> = super::StreamT<T, A>;
    }

    impl<T: Functor> Functor for Pairwise<T> {
        fn map<A: ExprCapable, B: ExprCapable>(
        ) -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>) {
            StreamT::<T>::map()
        }
    }

    impl<T: Applicative> Applicative for Pairwise<T> {
        // pure a = repeat (pure a)
        fn pure<A: ExprCapable>() -> Expr!(A => Self::Apply<A>) {
            repeat::<T, _>().compose(T::pure())
        }

        // map2 f = T::map2 (\(a:tas) (b:tbs) -> f a b : map2 f tas tbs)
        fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
        ) -> Expr!((A => B => C) => Self::Apply<A> => Self::Apply<B> => Self::Apply<C>) {
            Expression::new(FnType::new(|f| {
                letrec! {
                    let helper = Expression::new(fun!(|cons_a, cons_b| {
                        let (
                            Cons {
                                head: head_a,
                                tail: tail_a,
                            },
                            Cons {
                                head: head_b,
                                tail: tail_b,
                            },
                        ) = (DataExpr::destructure(cons_a), DataExpr::destructure(cons_b));
                        Cons {
                            head: f.apply(head_a).apply(head_b),
                            tail: result.apply(tail_a).apply(tail_b),
                        }
                    }));
                    let result = T::map2().apply(helper);
                    result.eval()
                }
            }))
        }
    }

    #[derive(Debug, Clone)]
    pub struct Cartesian<T: TypeCtor>(PhantomData<T>);

    impl<T: TypeCtor> ExprCapable for Cartesian<T> {}
    impl<T: TypeCtor> TypeCtor for Cartesian<T> {
        type Apply<A: ExprCapable> = super::StreamT<T, A>;
    }

    impl<T: Functor> Functor for Cartesian<T> {
        fn map<A: ExprCapable, B: ExprCapable>(
        ) -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>) {
            StreamT::<T>::map()
        }
    }

    impl<T: Applicative + Alt> Applicative for Cartesian<T> {
        // pure a = pure (a : none)
        fn pure<A: ExprCapable>() -> Expr!(A => Self::Apply<A>) {
            T::pure().compose(flip().apply(Cons::new()).apply(T::none()))
        }

        // map2 f = ap . map f
        fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
        ) -> Expr!((A => B => C) => Self::Apply<A> => Self::Apply<B> => Self::Apply<C>) {
            Expression::new(FnType::new(|f| {
                Self::ap().compose(Self::map().apply(f)).eval()
            }))
        }

        // ap = T::map2 (\(f:fs) (a:as) -> f a : (Self::map f as `concat` Self::map ($ a) fs `concat` Self::ap fs as))
        fn ap<A: ExprCapable, B: ExprCapable>(
        ) -> Expr!(Self::Apply<ExprType!(A => B)> => Self::Apply<A> => Self::Apply<B>) {
            T::map2().apply(Expression::new(FnType::new(|cons_f| {
                FnType::new(|cons_a| {
                    let (Cons { head: f, tail: fxs }, Cons { head: a, tail: axs }) =
                        (DataExpr::destructure(cons_f), DataExpr::destructure(cons_a));
                    Cons {
                        head: f.clone().apply(a.clone()),
                        tail: concat::<T, _>()
                            .apply(Self::map().apply(f).apply(axs.clone()))
                            .apply(
                                concat::<T, _>()
                                    .apply(Self::map().apply(call().apply(a)).apply(fxs.clone()))
                                    .apply(Self::ap().apply(fxs).apply(axs)),
                            ),
                    }
                })
            })))
        }
    }

    impl<T: Monad + Alt> Monad for Cartesian<T> {
        // bind f = join . map f
        fn bind<A: ExprCapable, B: ExprCapable>(
        ) -> Expr!((A => Self::Apply<B>) => Self::Apply<A> => Self::Apply<B>) {
            Expression::new(FnType::new(|f| {
                Self::join().compose(Self::map().apply(f)).eval()
            }))
        }

        // join = T::bind (\x -> (head x) `concat` (join (tail x)))
        // join = T::bind (combine concat head (join . tail))
        fn join<A: ExprCapable>() -> Expr!(Self::Apply<Self::Apply<A>> => Self::Apply<A>) {
            Expression::fix(FnType::new(|join| {
                T::bind()
                    .apply(
                        combine()
                            .apply(concat::<T, _>())
                            .apply(Cons::head())
                            .apply(join.compose(Cons::tail())),
                    )
                    .eval()
            }))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        control::Functor,
        data::Foldable,
        expression::{Expression, FnType},
        undefined,
    };

    use super::{instance::Stream, Cons};

    #[test]
    fn test_foldr() {
        let ascending = Expression::fix(FnType::new(|xs| Cons {
            head: Expression::new(0i32),
            tail: Stream::map()
                .apply(Expression::new(FnType::new(|x| x.eval() + 1)))
                .apply(xs),
        }));

        let sum = Stream::foldr()
            .apply(Expression::new(FnType::new(|x| {
                FnType::new(|b| {
                    let elem = x.eval();
                    if elem <= 10 {
                        elem + b.eval()
                    } else {
                        0
                    }
                })
            })))
            .apply(undefined())
            .apply(ascending);

        assert_eq!(sum.eval(), 55); // sum of 0..=10
    }
}
