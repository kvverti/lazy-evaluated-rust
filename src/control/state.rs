//! The [`MonadState`] type class and its canonical instance, the state monad transformer.

use crate::{
    Expr, Tup,
    control::{Applicative, Monad},
    data::{Type, pair::pair},
    funexp,
};

/// The `MonadState` type class defines monads that provide access to a changing state variable.
/// The canonical instance of this type class is the [`inst::StateT`] monad transformer.
pub trait MonadState<S: Type>: Monad {
    /// Get the current value of the state.
    fn get() -> Expr!(Self::Apply<S::Apply>);

    /// Set the state to a new value.
    fn set() -> Expr!(S::Apply => Self::Apply<()>);

    /// Update the state using the given pure function.
    fn update() -> Expr!((S::Apply => S::Apply) => Self::Apply<()>);
}

/// Updates state using an applicative update function.
pub fn update_a<S: Expr, T: Applicative>() -> Expr!((S => T::Apply<S>) => S => T::Apply<Tup!(S, ())>)
{
    funexp!(|f, s| T::map2()
        .apply(pair())
        .apply(f.apply(s))
        .apply(T::pure().apply_value(()))
        .eval())
}

pub mod inst {
    use std::marker::PhantomData;

    use crate::{
        Expr, ExprType, Tup,
        control::{
            Applicative, Functor, Monad, MonadFix, TypeCtor, identity::Identity, state::MonadState,
        },
        data::{
            Type,
            pair::{map_snd, snd},
        },
        expression::Expression,
        fun,
        function::compose,
        funexp, mdo,
    };

    /// The state monad transformer, which imbues a monad `T` with state. It is, of course,
    /// an instance of [`MonadState`].
    #[derive(Debug, Clone)]
    pub struct StateT<S: Type, T: TypeCtor>(PhantomData<(S, T)>);

    /// The state monad, which imbues pure computations with state.
    pub type State<S> = StateT<S, Identity>;

    impl<S: Type, T: TypeCtor> TypeCtor for StateT<S, T> {
        type Apply<A: Expr> = ExprType!(S::Apply => T::Apply<Tup!(S::Apply, A)>);
    }

    impl<S: Type, T: Functor> Functor for StateT<S, T> {
        // map f st s = map (map_snd f) (st s)
        // map f = (.) (map (map_snd f))
        fn map<A: Expr, B: Expr>() -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>) {
            compose().compose(T::map()).compose(map_snd())
        }
    }

    impl<S: Type, T: Monad> Applicative for StateT<S, T> {
        // pure a s = T::pure (s, a)
        fn pure<A: Expr>() -> Expr!(A => Self::Apply<A>) {
            funexp!(|a, s| T::pure().apply_value((s, a)).eval())
        }

        // map2 f fsa fsb s = T::do (
        //                      let (s1, a) = fsa s
        //                          (s2, b) = fsb s1
        //                      pure (s2, f a b)
        //                    )
        fn map2<A: Expr, B: Expr, C: Expr>()
        -> Expr!((A => B => C) => Self::Apply<A> => Self::Apply<B> => Self::Apply<C>) {
            funexp!(|f, sta, stb, s| mdo!({
                use T;
                let pat!((s, a)) = sta.apply(s);
                let pat!((s, b)) = stb.apply(s);
                return Expression::new((s, f.apply(a).apply(b)));
            })
            .eval())
        }
    }

    impl<S: Type, T: Monad> Monad for StateT<S, T> {
        // bind f fsa s = T::do (s1, a) = fsa s
        //                   f a s1
        fn bind<A: Expr, B: Expr>()
        -> Expr!((A => Self::Apply<B>) => Self::Apply<A> => Self::Apply<B>) {
            funexp!(|f, sta, s| mdo!({
                use T;
                let pat!((s, a)) = sta.apply(s);
                f.apply(a).apply(s)
            })
            .eval())
        }
    }

    // mfix :: (a -> s -> (s, a)) -> s -> (s, a)
    // mfix f s = fix (\(_, a) -> f a s)
    impl<S: Type, T: MonadFix> MonadFix for StateT<S, T> {
        fn mfix<A: Expr>(f: ExprType!(A => Self::Apply<A>)) -> Expr!(Self::Apply<A>) {
            funexp!(|s| T::mfix(fun!(|sa| f.apply(snd().apply(sa)).apply(s).eval())).eval())
        }
    }

    impl<S: Type, T: Monad> MonadState<S> for StateT<S, T> {
        fn get() -> Expr!(Self::Apply<S::Apply>) {
            funexp!(|s| T::pure().apply_value((s.clone(), s)).eval())
        }

        fn set() -> Expr!(S::Apply => Self::Apply<()>) {
            funexp!(|s, _| T::pure().apply_value((s, Expression::new(()))).eval())
        }

        fn update() -> Expr!((S::Apply => S::Apply) => Self::Apply<()>) {
            funexp!(|f, s| T::pure()
                .apply_value((f.apply(s), Expression::new(())))
                .eval())
        }
    }
}
