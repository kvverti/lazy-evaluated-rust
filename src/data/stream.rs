use crate::{
    Expr,
    control::{Alt, Functor, TypeCtor, identity::Identity},
    expression::{DataExpr, Expression},
    fun,
    function::{combine, flip},
    funexp, letrec,
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
pub struct Cons<T: TypeCtor, A: Expr> {
    head: Expression<A>,
    tail: Expression<StreamT<T, A>>,
}

impl<T: TypeCtor, A: Expr> Expr for Cons<T, A> {}
impl<T: TypeCtor, A: Expr> DataExpr for Cons<T, A> {
    fn destructure(v: Expression<Self>) -> Self {
        let v1 = v.clone();
        Self {
            head: Expression::lazy(|| v.eval().head.eval()),
            tail: Expression::lazy(|| v1.eval().tail.eval()),
        }
    }
}

impl<T: TypeCtor, A: Expr> Cons<T, A> {
    pub fn new() -> Expr!(A => StreamT<T, A> => Self) {
        funexp!(|head, tail| Self { head, tail })
    }

    pub fn head() -> Expr!(Self => A) {
        funexp!(|cons| {
            let Cons { head, tail: _ } = DataExpr::destructure(cons);
            head.eval()
        })
    }

    pub fn tail() -> Expr!(Self => StreamT<T, A>) {
        funexp!(|cons| {
            let Cons { head: _, tail } = DataExpr::destructure(cons);
            tail.eval()
        })
    }
}

// repeat ta = map (\a -> Cons a (repeat ta)) ta
// repeat ta = fix (\xs -> map (\a -> Cons a xs) ta)
pub fn repeat<T: Functor, A: Expr>() -> Expr!(T::Apply<A> => StreamT<T, A>) {
    funexp!(|elem| letrec!({
        let stream = T::map()
            .apply(flip().apply(Cons::new()).apply(stream))
            .apply(elem);
        stream.eval()
    }))
}

// flipconcat tys = rec0
//  where rec1 (x:txs') = Cons::new x (rec0 txs')
//        rec0 = flip alt tys . map rec1
pub fn concat<T: Functor + Alt, A: Expr>()
-> Expr!(StreamT<T, A> => StreamT<T, A> => StreamT<T, A>) {
    flip().apply_value(fun!(|tys| {
        letrec!({
            let rec0 = flip()
                .apply(T::alt())
                .apply(tys)
                .compose(T::map().apply(rec1));
            let rec1 = combine()
                .apply(Cons::new())
                .apply(Cons::head())
                .apply(rec0.compose(Cons::tail()));
            rec0.eval()
        })
    }))
}

pub mod instance {
    use std::marker::PhantomData;

    use crate::{
        Expr, ExprType,
        control::{Alt, Applicative, Functor, Monad, Traversable, TypeCtor, identity::Identity},
        data::Foldable,
        expression::DataExpr,
        fun,
        function::{call, combine, flip},
        funexp, letrec,
    };

    use super::{Cons, concat, repeat};

    #[derive(Debug, Clone)]
    pub struct StreamT<T: TypeCtor>(PhantomData<T>);

    pub type Stream = StreamT<Identity>;

    impl<T: TypeCtor> Expr for StreamT<T> {}
    impl<T: TypeCtor> TypeCtor for StreamT<T> {
        type Apply<A: Expr> = super::StreamT<T, A>;
    }

    impl<T: Functor> Functor for StreamT<T> {
        // map f = map (\(x:xs) -> f x : map f xs)
        // map f = map (combine new (f . head) (map f . tail))
        fn map<A: Expr, B: Expr>()
        -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>) {
            funexp!(|f| letrec!({
                let rec = T::map().apply(
                    combine()
                        .apply(Cons::new())
                        .apply(f.compose(Cons::head()))
                        .apply(rec.compose(Cons::tail())),
                );
                rec.eval()
            }))
        }
    }

    impl<T: Foldable> Foldable for StreamT<T> {
        // foldr f = T::foldr (\(x:txs) b -> (f x (Self::foldr f b txs)))
        // foldr f = rec0
        //     where rec0 = T::foldr rec
        //           rec (x:txs) b = f x (rec0 b txs)
        fn foldr<A: Expr, B: Expr>()
        -> Expr!((A => B => B) => B => Self::Apply<A> => B) {
            funexp!(|f| letrec!({
                let rec0 = T::foldr().apply(rec1);
                let rec1 = funexp!(|cons, b| {
                    let Cons { head, tail } = DataExpr::destructure(cons);
                    f.apply(head).apply(rec0.apply(b).apply(tail)).eval()
                });
                rec0.eval()
            }))
        }
    }

    impl<T: Traversable> Traversable for StreamT<T> {
        // traverse f = rec0
        //  where rec0 = T::traverse rec1
        //        rec1 (x:txs) = F::map2 Cons::new (f x) (rec0 txs)
        fn traverse<F: Applicative, A: Expr, B: Expr>()
        -> Expr!((A => F::Apply<B>) => Self::Apply<A> => F::Apply<Self::Apply<B>>) {
            funexp!(|f| letrec!({
                let rec0 = T::traverse::<F, _, _>().apply(rec1);
                let rec1 = combine()
                    .apply(F::map2().apply(Cons::new()))
                    .apply(f.compose(Cons::head()))
                    .apply(rec0.compose(Cons::tail()));
                rec0.eval()
            }))
        }
    }

    #[derive(Debug, Clone)]
    pub struct Pairwise<T: TypeCtor>(PhantomData<T>);

    impl<T: TypeCtor> Expr for Pairwise<T> {}
    impl<T: TypeCtor> TypeCtor for Pairwise<T> {
        type Apply<A: Expr> = super::StreamT<T, A>;
    }

    impl<T: Functor> Functor for Pairwise<T> {
        fn map<A: Expr, B: Expr>()
        -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>) {
            StreamT::<T>::map()
        }
    }

    impl<T: Applicative> Applicative for Pairwise<T> {
        // pure a = repeat (pure a)
        fn pure<A: Expr>() -> Expr!(A => Self::Apply<A>) {
            repeat::<T, _>().compose(T::pure())
        }

        // map2 f = T::map2 (\(a:tas) (b:tbs) -> f a b : map2 f tas tbs)
        fn map2<A: Expr, B: Expr, C: Expr>()
        -> Expr!((A => B => C) => Self::Apply<A> => Self::Apply<B> => Self::Apply<C>) {
            funexp!(|f| letrec!({
                let helper = funexp!(|cons_a, cons_b| {
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
                });
                let result = T::map2().apply(helper);
                result.eval()
            }))
        }
    }

    #[derive(Debug, Clone)]
    pub struct Cartesian<T: TypeCtor>(PhantomData<T>);

    impl<T: TypeCtor> Expr for Cartesian<T> {}
    impl<T: TypeCtor> TypeCtor for Cartesian<T> {
        type Apply<A: Expr> = super::StreamT<T, A>;
    }

    impl<T: Functor> Functor for Cartesian<T> {
        fn map<A: Expr, B: Expr>()
        -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>) {
            StreamT::<T>::map()
        }
    }

    impl<T: Applicative + Alt> Applicative for Cartesian<T> {
        // pure a = pure (a : none)
        fn pure<A: Expr>() -> Expr!(A => Self::Apply<A>) {
            T::pure().compose(flip().apply(Cons::new()).apply(T::none()))
        }

        // map2 f = ap . map f
        fn map2<A: Expr, B: Expr, C: Expr>()
        -> Expr!((A => B => C) => Self::Apply<A> => Self::Apply<B> => Self::Apply<C>) {
            funexp!(|f| Self::ap().compose(Self::map().apply(f)).eval())
        }

        // ap = T::map2 (\(f:fs) (a:as) -> f a : (Self::map f as `concat` Self::map ($ a) fs `concat` Self::ap fs as))
        fn ap<A: Expr, B: Expr>()
        -> Expr!(Self::Apply<ExprType!(A => B)> => Self::Apply<A> => Self::Apply<B>) {
            T::map2().apply_value(fun!(|cons_f, cons_a| {
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
            }))
        }
    }

    impl<T: Monad + Alt> Monad for Cartesian<T> {
        // bind f = join . map f
        fn bind<A: Expr, B: Expr>()
        -> Expr!((A => Self::Apply<B>) => Self::Apply<A> => Self::Apply<B>) {
            funexp!(|f| Self::join().compose(Self::map().apply(f)).eval())
        }

        // join = T::bind (\x -> (head x) `concat` (join (tail x)))
        // join = T::bind (combine concat head (join . tail))
        fn join<A: Expr>() -> Expr!(Self::Apply<Self::Apply<A>> => Self::Apply<A>) {
            letrec!({
                let join = T::bind().apply(
                    combine()
                        .apply(concat::<T, _>())
                        .apply(Cons::head())
                        .apply(join.compose(Cons::tail())),
                );
                join
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        control::Functor,
        data::Foldable,
        expression::Expression,
        fun, funexp, letrec, undefined,
    };

    use super::{Cons, instance::Stream};

    #[test]
    fn test_foldr() {
        let ascending = letrec!({
            let xs = Expression::new(Cons {
                head: Expression::new(0i32),
                tail: Stream::map().apply(funexp!(|x| x.eval() + 1)).apply(xs),
            });
            xs
        });

        let sum = Stream::foldr()
            .apply_value(fun!(|x, b| {
                let elem = x.eval();
                if elem <= 10 { elem + b.eval() } else { 0 }
            }))
            .apply(undefined())
            .apply(ascending);

        assert_eq!(sum.eval(), 55); // sum of 0..=10
    }
}
