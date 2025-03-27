use crate::{
    data::Foldable,
    expression::{ExprCapable, Expression, FnType},
    function::{compose, constant, id, flip},
    Expr, ExprType,
};

pub mod env;
pub mod identity;
pub mod state;
pub mod write;

/// Marks a type constructor, an abstraction for a generic type. Specifically, implementors
/// of this trait are stand-ins for a certain type-class view of a given type constructor. Multiple
/// distinct implementors may represent the same underlying generic type (e.g. [`Cartesian`] and [`Pairwise`] for streams).
/// 
/// [`Cartesian`]: crate::data::stream::instance::Cartesian
/// [`Pairwise`]: crate::data::stream::instance::Pairwise
pub trait TypeCtor: ExprCapable {
    type Apply<T: ExprCapable>: ExprCapable;
}

pub trait TypeCtor2: ExprCapable {
    type Apply<A: ExprCapable, B: ExprCapable>: ExprCapable;
}

/// Defines a monoidal type constructor. The [`Alt::none`] and [`Alt::alt`] operators are analogous to the
/// [`Monoid::empty`] and [`Associative::append`] operators, except that they are parametric. Additionally,
/// if the implementor is a [`Monad`], [`Alt::none`] is expected to be short-circuiting. That is, for all `f` and `m`,
/// `bind f none = none` and `sequence none m = sequence m none = none`.
/// 
/// This trait is based on the `Alternative` type class in Haskell [link, wiki link].
/// 
/// [`Monoid::empty`]: super::data::Monoid::empty
/// [`Associative::append`]: super::data::Associative::append
pub trait Alt: TypeCtor {
    // The empty element. This acts as an identity under [`Alt::alt`].
    fn none<A: ExprCapable>() -> Expr!(Self::Apply<A>);

    fn alt<A: ExprCapable>() -> Expr!(Self::Apply<A> => Self::Apply<A> => Self::Apply<A>);
}

/// A type constructor that allows lifting (single-argument) functions. The lifting operation should be
/// transparent to function composition. That is,
/// - `map id = id`
/// - `map (compose f g) = compose (map f) (map g)`
pub trait Functor: TypeCtor {
    fn map<A: ExprCapable, B: ExprCapable>() -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>);
}

pub trait ContraFunctor: TypeCtor {
    fn contramap<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((B => A) => Self::Apply<A> => Self::Apply<B>);
}

/// A [`Functor`] that additionally allows lifting function application. It can also be thought of
/// as a functor that can lift functions of any arity. In addition to satisfying the requirements of
/// [`Functor`], the [`Applicative::pure`] operator should be transparent to function application. That is,
/// - `map f (pure a) = pure (f a)`
pub trait Applicative: Functor {
    /// Lifts a value. This can also be thought of as lifting a zero-arity function.
    fn pure<A: ExprCapable>() -> Expr!(A => Self::Apply<A>);

    /// Lifts a binary function. This is the equivalent of [`Functor::map`] for functions with two arguments.
    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
    ) -> Expr!((A => B => C) => Self::Apply<A> => Self::Apply<B> => Self::Apply<C>);

    /// Lifts function application. This can also be thought of as a special case of [`map2`](Applicative::map2) with
    /// a function that applies its first argument to its second.
    fn ap<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!(Self::Apply<ExprType!(A => B)> => Self::Apply<A> => Self::Apply<B>) {
        Self::map2().apply(id())
    }
}

/// A scheme for composing functions that produce a context or effect in addition to a result.
/// Any monad is also an [`Applicative`] functor, and [`Applicative::pure`] is the identity element 
/// of [monadic composition](Monad::kleisli). That is, `kleisli f pure = kleisli pure f = f`.
/// 
/// Practically speaking, the underlying type constructor can be thought of as a context or effect, and the monadic
/// operators declared below define how to convey and sequence contexts wthout producing side effects.
pub trait Monad: Applicative {
    /// Lifts a contextual function. This is equivalent to [`Functor::map`] followed by [`join`](Monad::join)
    /// of the nested contexts.
    fn bind<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((A => Self::Apply<B>) => Self::Apply<A> => Self::Apply<B>);

    /// Collapses nested layers of context. This is equivalent to [`bind`](Monad::bind) applied to
    /// the identity function.
    fn join<A: ExprCapable>() -> Expr!(Self::Apply<Self::Apply<A>> => Self::Apply<A>) {
        Self::bind().apply(id())
    }

    /// Combines two contexts while ignoring any underlying values.
    fn sequence<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!(Self::Apply<B> => Self::Apply<A> => Self::Apply<B>) {
        compose().apply(Self::bind()).apply(constant())
    }

    /// Implements monadic function composition.
    /// 
    /// ## Why `kleisli`?
    /// This is analogous to the composition of morphisms in a kleisli category, which the category-theoretical
    /// monad construction operates with. Also, it's what Haskell calls it [link].
    // (f <=< g) a = f =<< g a
    fn kleisli<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
    ) -> Expr!((B => Self::Apply<C>) => (A => Self::Apply<B>) => A => Self::Apply<C>) {
        compose().apply(compose()).apply(Self::bind())
    }
}

/// Defines the monadic fix point operator. A fixed point of a monadic function `f` is a monadic
/// expression `mx` such that `bind f mx = mx`. Like the normal [`fix`],
/// if `f` is strict in its argument then the result is undefined.
/// 
/// ## Why not use `fix (bind f)`?
/// Becuase for many data structures, [`Monad::bind`] is strict in its second argument, and so doesn't work with `fix`!
/// Implementations of [`Monad::bind`] don't know whether there is recursion involved, so they may have to eagerly
/// evaluate values that end up unused later. Implementations of `mfix` can evaluate lazily, when some
/// later step of the recursion actually uses the prior computation.
/// 
/// For more information, see [Haskell wiki link, MonadFix paper].
/// 
/// [`fix`]: crate::fix
pub trait MonadFix: Monad {
    fn mfix<A: ExprCapable>() -> Expr!((A => Self::Apply<A>) => Self::Apply<A>);
}

/// The dual of a [`Monad`]. It can be thought of as a scheme for composing functions that take a context or
/// effect in addition to an argument. Analogous to [`Monad`], [`Comonad::extract`] is the identity element
/// of comonadic composition.
pub trait Comonad: Functor {
    /// Extracts a value from the comonadic context. This is the dual of [`Applicative::pure`]. A comonad
    /// can often be thought of as storing a special "focus" element, and this function extracts that element.
    fn extract<A: ExprCapable>() -> Expr!(Self::Apply<A> => A);

    fn extend<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((Self::Apply<A> => B) => Self::Apply<A> => Self::Apply<B>);

    fn duplicate<A: ExprCapable>() -> Expr!(Self::Apply<A> => Self::Apply<Self::Apply<A>>) {
        Self::extend().apply(id())
    }

    /// Implements comonadic function composition. It's called "cokleisli" for the same reason monadic
    /// function composition is called [kleisli](Monad::kleisli).
    // (f =<= g) wa = f (g <<= wa)
    fn cokleisli<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
    ) -> Expr!((Self::Apply<B> => C) => (Self::Apply<A> => B) => Self::Apply<A> => C) {
        Expression::new(FnType::new(|f| {
            FnType::new(|g| f.compose(Self::extend().apply(g)).eval())
        }))
    }
}

/// A structure that can be pulled out of an [`Applicative`] operator.
pub trait Traversable: Functor + Foldable {
    fn traverse<F: Applicative, A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((A => F::Apply<B>) => Self::Apply<A> => F::Apply<Self::Apply<B>>);

    fn sequence<F: Applicative, A: ExprCapable>(
    ) -> Expr!(Self::Apply<F::Apply<A>> => F::Apply<Self::Apply<A>>) {
        Self::traverse::<F, _, _>().apply(id())
    }
}

pub trait Profunctor: TypeCtor2 {
    fn dimap<A1: ExprCapable, A2: ExprCapable, B1: ExprCapable, B2: ExprCapable>() -> Expr!((A2 => A1) => (B1 => B2) => Self::Apply<A1, B1> => Self::Apply<A2, B2>);

    fn map_left<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expr!((B => A) => Self::Apply<A, C> => Self::Apply<B, C>) {
        flip().apply(Self::dimap()).apply(id())
    }

    fn map_right<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expr!((A => B) => Self::Apply<C, A> => Self::Apply<C, B>) {
        Self::dimap().apply(id())
    }
}
