//! The exception typeclass and its canonical implementations, the [`inst::Except`] monad
//! and [`inst::ExceptT`] monad transformer.

use crate::{
    Expr,
    control::Monad,
    data::Type,
    expression::{DataExpr, Expression},
};

/// The `MonadExcept` type class defines monads that can short-circuit with some
/// context type `E`.
pub trait MonadExcept<E: Type>: Monad {
    /// Escape the current computation with the given context.
    fn escape<A: Expr>() -> Expr!(E::Apply => Self::Apply<A>);
}

/// The canonical implementation of exceptions. It can either represent a pure value with
/// [`Except::Continue`], or an exception with [`Except::Break`]. Note that, unlike in Haskell,
/// this type is monadic over the first argument.
#[derive(Debug, Clone)]
pub enum Except<A: Expr, E: Expr> {
    Continue(Expression<A>),
    Break(Expression<E>),
}

impl<A: Expr, E: Expr> Expr for Except<A, E> {}

impl<A: Expr, E: Expr> DataExpr for Except<A, E> {
    fn destructure(v: Expression<Self>) -> Self {
        v.eval()
    }
}

/// Monad instances for exceptions.
pub mod inst {
    use std::marker::PhantomData;

    use super::Except::{Break, Continue};
    use crate::{
        Expr, ExprType,
        control::{
            Applicative, Functor, Monad, MonadFix, Traversable, TypeCtor, except::MonadExcept,
        },
        data::{Foldable, Type, compose::Compose},
        dorec,
        expression::{DataExpr, Expression},
        fun, funexp, letrec, mdo,
    };

    /// The exception monad with the exception type `E`. This monad is, of course,
    /// also an instance of [`MonadExcept`].
    #[derive(Debug, Clone)]
    pub struct Except<E: Type>(PhantomData<E>);

    impl<E: Type> Expr for Except<E> {}
    impl<E: Type> TypeCtor for Except<E> {
        type Apply<T: Expr> = super::Except<T, E::Apply>;
    }

    impl<E: Type> Foldable for Except<E> {
        fn foldr<A: Expr, B: Expr>() -> Expr!((A => B => B) => B => Self::Apply<A> => B) {
            funexp!(|f, b, ex| match DataExpr::destructure(ex) {
                Continue(a) => f.apply(a).apply(b).eval(),
                Break(_) => b.eval(),
            })
        }

        fn foldl_strict<A: Expr, B: Expr>() -> Expr!((B => A => B) => B => Self::Apply<A> => B) {
            funexp!(|f, b, ex| match DataExpr::destructure(ex) {
                Continue(a) => f.apply(b).apply(a).eval(),
                Break(_) => b.eval(),
            })
        }
    }

    impl<E: Type> Traversable for Except<E> {
        fn traverse<F: Applicative, A: Expr, B: Expr>()
        -> Expr!((A => F::Apply<B>) => Self::Apply<A> => F::Apply<Self::Apply<B>>) {
            funexp!(|f, ex| match DataExpr::destructure(ex) {
                Continue(a) => F::map()
                    .apply_value(fun!(|a| Continue(a)))
                    .apply(f.apply(a))
                    .eval(),
                Break(e) => F::pure().apply_value(Break(e)).eval(),
            })
        }
    }

    impl<E: Type> Functor for Except<E> {
        fn map<A: Expr, B: Expr>() -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>) {
            funexp!(|f, ma| match DataExpr::destructure(ma) {
                Continue(a) => Continue(f.apply(a)),
                Break(e) => Break(e),
            })
        }
    }

    impl<E: Type> Applicative for Except<E> {
        fn pure<A: Expr>() -> Expr!(A => Self::Apply<A>) {
            funexp!(|a| Continue(a))
        }

        fn map2<A: Expr, B: Expr, C: Expr>()
        -> Expr!((A => B => C) => Self::Apply<A> => Self::Apply<B> => Self::Apply<C>) {
            funexp!(|f, ma, mb| match DataExpr::destructure(ma) {
                Continue(a) => match DataExpr::destructure(mb) {
                    Continue(b) => Continue(f.apply(a).apply(b)),
                    Break(e) => Break(e),
                },
                Break(e) => Break(e),
            })
        }

        fn ap<A: Expr, B: Expr>()
        -> Expr!(Self::Apply<ExprType!(A => B)> => Self::Apply<A> => Self::Apply<B>) {
            funexp!(|mf, ma| match DataExpr::destructure(mf) {
                Break(e) => Break(e),
                Continue(f) => match DataExpr::destructure(ma) {
                    Continue(a) => Continue(f.apply(a)),
                    Break(e) => Break(e),
                },
            })
        }
    }

    impl<E: Type> Monad for Except<E> {
        fn bind<A: Expr, B: Expr>()
        -> Expr!((A => Self::Apply<B>) => Self::Apply<A> => Self::Apply<B>) {
            funexp!(|f, ma| match DataExpr::destructure(ma) {
                Continue(a) => f.apply(a).eval(),
                Break(e) => Break(e),
            })
        }

        fn join<A: Expr>() -> Expr!(Self::Apply<Self::Apply<A>> => Self::Apply<A>) {
            funexp!(|mma| match DataExpr::destructure(mma) {
                Continue(ma) => ma.eval(),
                Break(e) => Break(e),
            })
        }
    }

    impl<E: Type> MonadFix for Except<E> {
        fn mfix<A: Expr>(f: ExprType!(A => Self::Apply<A>)) -> Expr!(Self::Apply<A>) {
            letrec!({
                let ex = f.apply(Expression::lazy(|| match DataExpr::destructure(ex) {
                    Continue(a) => a.eval(),
                    Break(e) => crate::undefined().eval(),
                }));
                ex
            })
        }
    }

    impl<E: Type> MonadExcept<E> for Except<E> {
        fn escape<A: Expr>() -> Expr!(E::Apply => Self::Apply<A>) {
            funexp!(|e| Break(e))
        }
    }

    /// The exception monad transformer, which adds exceptions to a given monad `M`. The
    /// resultant monad, of course, is an instance of [`MonadExcept`].
    #[derive(Debug, Clone)]
    pub struct ExceptT<E: Type, M: TypeCtor>(PhantomData<(E, M)>);

    impl<E: Type, M: TypeCtor> Expr for ExceptT<E, M> {}
    impl<E: Type, M: TypeCtor> TypeCtor for ExceptT<E, M> {
        type Apply<T: Expr> = <Compose<M, Except<E>> as TypeCtor>::Apply<T>;
    }

    impl<E: Type, M: Functor> Functor for ExceptT<E, M> {
        fn map<A: Expr, B: Expr>() -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>) {
            <Compose<M, Except<E>>>::map()
        }
    }

    impl<E: Type, M: Applicative> Applicative for ExceptT<E, M> {
        fn pure<A: Expr>() -> Expr!(A => Self::Apply<A>) {
            <Compose<M, Except<E>>>::pure()
        }

        fn map2<A: Expr, B: Expr, C: Expr>()
        -> Expr!((A => B => C) => Self::Apply<A> => Self::Apply<B> => Self::Apply<C>) {
            <Compose<M, Except<E>>>::map2()
        }

        fn ap<A: Expr, B: Expr>()
        -> Expr!(Self::Apply<ExprType!(A => B)> => Self::Apply<A> => Self::Apply<B>) {
            <Compose<M, Except<E>>>::ap()
        }
    }

    impl<E: Type, M: Monad> Monad for ExceptT<E, M> {
        fn bind<A: Expr, B: Expr>()
        -> Expr!((A => Self::Apply<B>) => Self::Apply<A> => Self::Apply<B>) {
            funexp!(|f, ma| mdo!({
                use M;
                let ex = ma;
                match DataExpr::destructure(ex) {
                    Continue(a) => f.apply(a),
                    Break(e) => M::pure().apply_value(Break(e)),
                }
            })
            .eval())
        }

        fn join<A: Expr>() -> Expr!(Self::Apply<Self::Apply<A>> => Self::Apply<A>) {
            funexp!(|mma| mdo!({
                use M;
                let ex = mma;
                match DataExpr::destructure(ex) {
                    Continue(ma) => ma,
                    Break(e) => M::pure().apply_value(Break(e)),
                }
            })
            .eval())
        }
    }

    impl<E: Type, M: MonadFix> MonadFix for ExceptT<E, M> {
        fn mfix<A: Expr>(f: ExprType!(A => Self::Apply<A>)) -> Expr!(Self::Apply<A>) {
            dorec!({
                use M;
                let ex = f.apply(Expression::lazy(|| match DataExpr::destructure(ex) {
                    Continue(a) => a.eval(),
                    Break(_) => crate::undefined().eval(),
                }));
                return ex;
            })
        }
    }

    impl<E: Type, M: Monad> MonadExcept<E> for ExceptT<E, M> {
        fn escape<A: Expr>() -> Expr!(E::Apply => Self::Apply<A>) {
            M::pure().compose(<Except<E>>::escape())
        }
    }
}
