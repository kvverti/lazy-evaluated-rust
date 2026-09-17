//! The [`MonadState`] type class and its canonical instance, the state monad transformer.

use crate::{ExprType, Tup};

use super::TypeCtor;

pub type StateFn<S, T, A> = ExprType!(S => type <T as TypeCtor>::Apply<Tup!(S, A)>);

pub mod inst {
    use std::marker::PhantomData;

    use crate::{
        Expr, ExprType,
        control::{
            Applicative, Functor, Monad, MonadFix, TypeCtor, identity::Identity, state::StateFn,
        },
        data::{
            Type,
            pair::{map_snd, pair, snd},
        },
        expression::Expression,
        fun,
        function::{compose, constant, dup},
        funexp, mdo,
    };

    #[derive(Debug, Clone)]
    pub struct StateT<S: Type, T: TypeCtor>(PhantomData<(S, T)>);
    pub type State<S> = StateT<S, Identity>;

    /// Gets the state value.
    pub fn get<S: Expr, T: Applicative>() -> Expr!(StateFn<S, T, S>) {
        T::pure().compose(dup().apply(pair()))
    }

    /// Sets the state value.
    pub fn set<S: Expr, T: Applicative>() -> Expr!(S => StateFn<S, T, ()>) {
        funexp!(|s| constant()
            .apply(T::pure().apply_value((s, Expression::new(()))))
            .eval())
    }

    /// Updates the state value using an applicative update function.
    pub fn update_a<S: Expr, T: Applicative>() -> Expr!((S => T::Apply<S>) => StateFn<S, T, ()>) {
        funexp!(|f, s| T::map2()
            .apply(pair())
            .apply(f.apply(s))
            .apply(T::pure().apply_value(()))
            .eval())
    }

    /// Updates the state value using a pure update function.
    pub fn update<S: Expr, T: Applicative>() -> Expr!((S => S) => StateFn<S, T, ()>) {
        // update = updateA . (pure .)
        update_a::<S, T>().compose(compose().apply(T::pure()))
    }

    impl<S: Type, T: TypeCtor> Expr for StateT<S, T> {}

    impl<S: Type, T: TypeCtor> TypeCtor for StateT<S, T> {
        type Apply<A: Expr> = StateFn<S::Apply, T, A>;
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

        fn ap<A: Expr, B: Expr>()
        -> Expr!(Self::Apply<ExprType!(A => B)> => Self::Apply<A> => Self::Apply<B>) {
            Self::map2().apply(crate::function::id())
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
}
