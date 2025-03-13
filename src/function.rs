use crate::{
    expression::{ExprCapable, Expression, FnType},
    Expr,
};

/// The identity function for the type `T`. It returns its input unchanged.
pub fn id<T: ExprCapable>() -> Expr!(T => T) {
    Expression::new(FnType::new(|x| x.eval()))
}

/// The constant function. It produces a function that ignores its input and evaluates to
/// the provided value.
pub fn constant<T: ExprCapable, R: ExprCapable>() -> Expr!(R => T => R) {
    Expression::new(FnType::new(|c| FnType::new(|_| c.eval())))
}

/// Composes two functions `f` and `g`, producing a function that evaluates `x` to `f(g(x))`.
pub fn compose<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
) -> Expr!((B => C) => (A => B) => A => C) {
    Expression::new(FnType::new(|f| FnType::new(|g| f.compose(g).eval())))
}

pub fn combine<A: ExprCapable, B: ExprCapable, C: ExprCapable, D: ExprCapable>(
) -> Expr!((A => B => C) => (D => A) => (D => B) => D => C) {
    Expression::new(FnType::new(|f| {
        FnType::new(|g| {
            FnType::new(|h| FnType::new(|x| f.apply(g.apply(x.clone())).apply(h.apply(x)).eval()))
        })
    }))
}
