//! The uninhabited [`Void`] type and its instances.

use crate::{Expr, data::Associative, expression::DataExpr, function::constant, funexp};

/// An uninhabited type. This is an alias for `!`.
pub type Void = core::convert::Infallible;

impl DataExpr for Void {
    fn destructure(v: crate::expression::Expression<Self>) -> Self {
        v.eval()
    }
}

/// A value of type `Void` may be transformed into any value. This function is,
/// of course, strict in its argument, so passing a diverging expression of `Void`
/// type will make this function diverge.
pub fn absurd<A: Expr>() -> Expr!(Void => A) {
    funexp!(|v| match DataExpr::destructure(v) {})
}

/// While `Void` is not a [`Monoid`] (due to having no inhabitants), it is associative.
/// This implementation is strict in its second argument.
impl Associative for Void {
    fn append() -> Expr!(Self::Apply => Self::Apply => Self::Apply) {
        constant().apply(absurd())
    }
}
