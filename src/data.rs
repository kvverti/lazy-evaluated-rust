use crate::{
    control::TypeCtor,
    expression::{ExprCapable, Expression, FnType},
    function::id,
    Expr,
};

pub mod compose;
pub mod constant;
pub mod list;
pub mod maybe;
pub mod pair;
pub mod stream;

#[macro_export]
macro_rules! Tup {
    ($($elem:ty),+ $(,)?) => {
        ($($crate::expression::Expression<$elem>,)*)
    };
}

pub use Tup;

pub trait Newtype: ExprCapable {
    type Inner: ExprCapable;

    fn lift(v: Expression<Self::Inner>) -> Self;
    fn unlift(v: Expression<Self>) -> Self::Inner;
}

pub fn coerce<T: Newtype, U: Newtype<Inner = T::Inner>>() -> Expr!(T => U) {
    Expression::new(FnType::new(|t| U::lift(FnType::new(T::unlift).apply(t))))
}

pub trait Type: ExprCapable {
    type Apply: ExprCapable;
}

/// A trait for types that define an associative binary operator.
/// E.g. addition over the integers or list concatenation.
pub trait Associative: Type {
    /// The associative binary operation.
    // todo: try implementing instances for type tokens that map to the actual type (similar to type constructors)
    fn append() -> Expr!(Self::Apply => Self::Apply => Self::Apply);
}

/// A trait for associative types that also have an identity element.
pub trait Monoid: Associative {
    /// The identity element of the associative operator. This should satisfy
    /// `append(empty, a) == append(a, empty) == a`.
    fn empty() -> Expr!(Self::Apply);
}

pub trait Foldable: TypeCtor {
    fn foldr<A: ExprCapable, B: ExprCapable>() -> Expr!((A => B => B) => B => Self::Apply<A> => B);

    fn foldl_strict<A: ExprCapable, B: ExprCapable>(
    ) -> Expr!((B => A => B) => B => Self::Apply<A> => B) {
        Expression::new(FnType::new(|f| {
            FnType::new(|b| {
                FnType::new(|this| {
                    Self::foldr()
                        .apply(Expression::new(FnType::new(|a| {
                            FnType::new(|g| {
                                FnType::new(|b| g.apply_strict(f.apply(b).apply(a)).eval())
                            })
                        })))
                        .apply(id())
                        .apply(this)
                        .apply(b)
                        .eval()
                })
            })
        }))
    }

    // fold_map f = foldr (append . f) empty
    fn fold_map<M: Monoid, A: ExprCapable>() -> Expr!((A => M::Apply) => Self::Apply<A> => M::Apply)
    {
        Expression::new(FnType::new(|f| {
            Self::foldr()
                .apply(M::append().compose(f))
                .apply(M::empty())
                .eval()
        }))
    }

    fn fold<M: Monoid, A: ExprCapable>() -> Expr!(Self::Apply<M::Apply> => M::Apply) {
        Self::fold_map::<M, _>().apply(id())
    }
}
