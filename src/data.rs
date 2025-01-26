use crate::{
    control::TypeCtor,
    expression::{ExprCapable, Expression, FnType},
    id,
};

pub mod compose;
pub mod constant;
pub mod list;
pub mod maybe;

pub trait Newtype<T>: Sized {
    fn lift(v: Expression<T>) -> Self;
    fn unlift(v: Expression<Self>) -> T;
}

/// A trait for types that define an associative binary operator.
/// E.g. addition over the integers or list concatenation.
pub trait Associative: ExprCapable {
    /// The associative binary operation.
    fn append() -> Expression<FnType<Self, FnType<Self, Self>>>;
}

/// A trait for associative types that also have an identity element.
pub trait Monoid: Associative {
    /// The identity element of the associative operator. This should satisfy
    /// `append(empty, a) == append(a, empty) == a`.
    fn empty() -> Expression<Self>;
}

pub trait Foldable: TypeCtor {
    fn foldr<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, FnType<B, B>>, FnType<B, FnType<Self::Apply<A>, B>>>>;

    fn foldl_strict<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<B, FnType<A, B>>, FnType<B, FnType<Self::Apply<A>, B>>>> {
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
}
