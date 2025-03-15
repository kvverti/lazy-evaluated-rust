use crate::{
    control::{identity::Identity, Functor, TypeCtor},
    expression::{DataExpr, ExprCapable, Expression, FnType},
    function::flip,
    Expr,
};

pub type StreamT<T, A> = <T as TypeCtor>::Apply<Cons<T, A>>;

pub type Stream<A> = StreamT<Identity, A>;

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

pub mod instance {
    use std::marker::PhantomData;

    use crate::{
        control::{Applicative, Functor, TypeCtor},
        expression::{DataExpr, ExprCapable, Expression, FnType},
        function::combine,
        Expr,
    };

    use super::{repeat, Cons};

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
}
