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

// combine f g = s (f . g)
pub fn combine<A: ExprCapable, B: ExprCapable, C: ExprCapable, D: ExprCapable>(
) -> Expr!((A => B => C) => (D => A) => (D => B) => D => C) {
    Expression::new(FnType::new(|f| {
        FnType::new(|g| s().apply(f.compose(g)).eval())
    }))
}

// s f g a = f a (g a)
pub fn s<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
) -> Expr!((A => B => C) => (A => B) => A => C) {
    Expression::new(FnType::new(|f| {
        FnType::new(|g| FnType::new(|x| f.apply(x.clone()).apply(g.apply(x)).eval()))
    }))
}

/// Reverses the order of the arguments of a binary function.
pub fn flip<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expr!((A => B => C) => B => A => C)
{
    Expression::new(FnType::new(|f| {
        FnType::new(|b| FnType::new(|a| f.apply(a).apply(b).eval()))
    }))
}

pub fn apply<A: ExprCapable, B: ExprCapable>() -> Expr!((A => B) => A => B) {
    id()
}

pub fn call<A: ExprCapable, B: ExprCapable>() -> Expr!(A => (A => B) => B) {
    flip().apply(apply())
}

// dup f x = f x x
pub fn dup<A: ExprCapable, B: ExprCapable>() -> Expr!((A => A => B) => A => B) {
    Expression::new(FnType::new(|f| {
        FnType::new(|x| f.apply(x.clone()).apply(x).eval())
    }))
}
