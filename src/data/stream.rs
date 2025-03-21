use crate::{
    control::{identity::Identity, Alt, Functor, TypeCtor},
    expression::{DataExpr, ExprCapable, Expression, FnType},
    function::flip,
    Expr,
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
            head: Expression::lazy(move || v.eval().head.eval()),
            tail: Expression::lazy(move || v1.eval().tail.eval()),
        }
    }
}

impl<T: TypeCtor, A: ExprCapable> Cons<T, A> {
    pub fn new() -> Expr!(A => StreamT<T, A> => Self) {
        Expression::new(FnType::new(|head| FnType::new(|tail| Self { head, tail })))
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

// concat txs tys = alt (map (\(x:txs') -> x : concat txs' tys) txs) tys
pub fn concat<T: Functor + Alt, A: ExprCapable>(
) -> Expr!(StreamT<T, A> => StreamT<T, A> => StreamT<T, A>) {
    Expression::fix(FnType::new(|rec| {
        FnType::new(|stream1| {
            FnType::new(|stream2| {
                T::alt()
                    .apply(
                        T::map()
                            .apply(Expression::new(FnType::new({
                                let stream2 = stream2.clone();
                                |cons1| {
                                    let Cons { head, tail } = DataExpr::destructure(cons1);
                                    Cons {
                                        head,
                                        tail: rec.apply(tail).apply(stream2),
                                    }
                                }
                            })))
                            .apply(stream1),
                    )
                    .apply(stream2)
                    .eval()
            })
        })
    }))
}

pub mod instance {
    use std::marker::PhantomData;

    use crate::{
        control::{Alt, Applicative, Functor, Monad, TypeCtor},
        expression::{DataExpr, ExprCapable, Expression, FnType},
        function::{call, combine, flip},
        Expr, ExprType,
    };

    use super::{concat, repeat, Cons};

    #[derive(Debug, Clone)]
    pub struct StreamT<T: TypeCtor>(PhantomData<T>);

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
                Expression::fix(FnType::new(|rec| {
                    T::map2()
                        .apply(Expression::new(FnType::new(|cons_a| {
                            FnType::new(|cons_b| {
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
                                    tail: rec.apply(tail_a).apply(tail_b),
                                }
                            })
                        })))
                        .eval()
                }))
                .eval()
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
