use std::{fmt::Debug, marker::PhantomData, sync::Arc};

use once_cell::sync::Lazy;

mod macros;

/// Trait for any type that supports being lazily evaluated.
/// This includes types that have no internal structure as well
/// as user-defined types that contain Expressions.
pub trait ExprCapable: Clone + 'static {}

/// Trait for lazy data structures.
pub trait DataExpr: ExprCapable {
    /// Lazily destructure a data structure into its component fields.
    /// This should be fully lazy for single-variant data, but for multi-variant data
    /// this evaluates the tag. In neither case are the fields evaluated.
    fn destructure(v: Expression<Self>) -> Self;
}

macro_rules! impl_primitive {
    ($($t:ty)*) => {
        $( impl ExprCapable for $t {} )*
    };
}

impl_primitive!(() u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize char bool);

macro_rules! impl_tuple {
    ($($t:ident $n:tt)*) => {
        impl<$($t : ExprCapable),*> ExprCapable for ( $(Expression<$t>,)* ) {}
        impl<$($t : ExprCapable),*> DataExpr for ( $(Expression<$t>,)* ) {
            fn destructure(v: Expression<Self>) -> Self {
                (
                    $( Expression::lazy({ let v = v.clone(); move || v.eval_ref().$n.eval_ref().clone() }), )*
                )
            }
        }
    };

    ($( ($($t:tt)*) )*) => {
        $( impl_tuple!($($t)*); )*
    }
}

impl_tuple!(
    (T0 0)
    (T0 0 T1 1)
    (T0 0 T1 1 T2 2)
    (T0 0 T1 1 T2 2 T3 3)
    (T0 0 T1 1 T2 2 T3 3 T4 4)
    (T0 0 T1 1 T2 2 T3 3 T4 4 T5 5)
    (T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6)
    (T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7)
    (T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8)
    (T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9)
    (T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10)
    (T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10 T11 11)
    (T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10 T11 11 T12 12)
    (T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10 T11 11 T12 12 T13 13)
    (T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10 T11 11 T12 12 T13 13 T14 14)
    (T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10 T11 11 T12 12 T13 13 T14 14 T15 15)
    (T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10 T11 11 T12 12 T13 13 T14 14 T15 15 T16 16)
);

impl<T: ExprCapable, R: ExprCapable> ExprCapable for FnType<T, R> {}
impl<T: ExprCapable> ExprCapable for PhantomData<T> {}
impl<T: ExprCapable, const N: usize> ExprCapable for [Expression<T>; N] {}

impl<T: ExprCapable, const N: usize> DataExpr for [Expression<T>; N] {
    fn destructure(v: Expression<Self>) -> Self {
        core::array::from_fn(|idx| v.clone().map(move |arr| arr[idx].eval_ref().clone()))
    }
}

#[macro_export]
macro_rules! ExprType {
    (type $ty:ty) => { $ty };
    ($tyname:path) => { $tyname };
    ($arg:path => $($ret:tt)*) => { $crate::expression::FnType<$crate::ExprType!($arg), $crate::ExprType!($($ret)*)> };
    ($arg:tt => $($ret:tt)*) => { $crate::expression::FnType<$crate::ExprType!($arg), $crate::ExprType!($($ret)*)> };
    (($($t:tt)*)) => { $crate::ExprType!($($t)*) };
}

#[macro_export]
macro_rules! Expr {
    ($($t:tt)*) => {
        $crate::expression::Expression<$crate::ExprType!($($t)*)>
    };
}

/// A lazily evaluated expression that evaluates to a value of type `T`.
/// Expressions can be constructed from a value directly, or (more commonly)
/// from a thunk. Function expressions can be applied lazily, and data expressions
/// can be matched against lazily.
///
/// Interestingly, `Self::map`, `Self::eval`, and `Self::apply` form a comonad with `counit = eval` and `cobind = apply`.
#[derive(Debug)]
pub struct Expression<T> {
    value: Arc<Lazy<T, Box<dyn FnOnce() -> T>>>,
}

impl<T: ExprCapable> Expression<T> {
    /// Construct an expression that evaluates to the given value.
    pub fn new(value: T) -> Self {
        Self::lazy(|| value)
    }

    /// Construct an expression that runs the given thunk when evaluated.
    pub fn lazy(f: impl FnOnce() -> T + 'static) -> Self {
        Self {
            value: Arc::new(Lazy::new(Box::new(f))),
        }
    }

    /// Call the provided function with its own return expression. This effectively
    /// exposes sharing recursion. Also known as the fixed point combinator.
    pub fn fix(f: FnType<T, T>) -> Self {
        Self {
            value: Arc::new_cyclic(move |value| {
                let value = value.clone();
                // pass our return expression as the argument to the function
                // currently, this leaks `self.value`
                Lazy::new(Box::new(move || {
                    f.0.call(Expression {
                        value: value.upgrade().expect("value should not be dropped"),
                    })
                }) as _)
            }),
        }
    }

    /// Evaluates this expression and returns a reference to the underlying value.
    pub fn eval_ref(&self) -> &T {
        &self.value
    }

    /// Evaluates this expression and returns a clone of the underlying value.
    pub fn eval(self) -> T {
        // evaluate the expression now to prevent early drop in the case of a reference cycle
        Lazy::force(&self.value);
        match Arc::try_unwrap(self.value) {
            Ok(lazy) => Lazy::into_value(lazy)
                .unwrap_or_else(|_| panic!("expression should have been evaluated")),
            Err(arc) => (**arc).clone(),
        }
    }

    /// Returns an expression that evaluates to the value of this expression applied to the given function.
    pub fn map<R: ExprCapable>(self, f: impl FnOnce(T) -> R + 'static) -> Expression<R> {
        Expression::lazy(move || f(self.eval()))
    }
}

impl<T: ExprCapable, R: ExprCapable> Expression<FnType<T, R>> {
    /// Lazily call this function with the given argument. When evaluated,
    /// this evaluates `self` and calls it with `arg`.
    pub fn apply(self, arg: Expression<T>) -> Expression<R> {
        Expression::lazy(|| self.eval().call(arg))
    }

    /// Call this function with the given argument. This method uses strict (call-by-value)
    /// semantics; the argument will always be evaluated.
    pub fn apply_strict(self, arg: Expression<T>) -> Expression<R> {
        Expression::lazy(|| {
            Lazy::force(&arg.value);
            self.eval().call(arg)
        })
    }

    /// Compose this function with the given function.
    pub fn compose<U: ExprCapable>(self, f: Expression<FnType<U, T>>) -> Expression<FnType<U, R>> {
        f.map(|f| self.eval().compose(f))
    }
}

// comparison operators for expressions
// these are all strict in both arguments.

impl<T: ExprCapable + PartialEq> PartialEq for Expression<T> {
    fn eq(&self, other: &Self) -> bool {
        self.eval_ref() == other.eval_ref()
    }
}

impl<T: ExprCapable + Eq> Eq for Expression<T> {}

impl<T: ExprCapable + PartialOrd> PartialOrd for Expression<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.eval_ref().partial_cmp(other.eval_ref())
    }
}

impl<T: ExprCapable + Ord> Ord for Expression<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.eval_ref().cmp(other.eval_ref())
    }
}

/// Trait for functions that can be stored in lazily evaluated expressions.
pub trait FnCapable<T, R>: 'static {
    fn call(self: Box<Self>, v: Expression<T>) -> R;

    fn dyn_clone(&self) -> Box<dyn FnCapable<T, R>>;
}

impl<T, R, F> FnCapable<T, R> for F
where
    F: 'static + Clone + FnOnce(Expression<T>) -> R,
{
    fn dyn_clone(&self) -> Box<dyn FnCapable<T, R>> {
        Box::new(self.clone())
    }

    fn call(self: Box<Self>, v: Expression<T>) -> R {
        self(v)
    }
}

pub struct FnType<T, R>(Box<dyn FnCapable<T, R>>);

impl<T: ExprCapable, R: ExprCapable> FnType<T, R> {
    pub fn new(f: impl FnOnce(Expression<T>) -> R + Clone + 'static) -> Self {
        Self(Box::new(f))
    }

    pub fn call(self, arg: Expression<T>) -> R {
        self.0.call(arg)
    }

    /// Lazily call this function with the given argument. When evaluated,
    /// this evaluates `self` and calls it with `arg`.
    pub fn apply(self, arg: Expression<T>) -> Expression<R> {
        Expression::lazy(|| self.call(arg))
    }

    /// Call this function with the given argument. This method uses strict (call-by-value)
    /// semantics; the argument will always be evaluated.
    pub fn apply_strict(self, arg: Expression<T>) -> Expression<R> {
        Expression::lazy(|| {
            Lazy::force(&arg.value);
            self.call(arg)
        })
    }

    /// Compose this function with the given function.
    pub fn compose<U: ExprCapable>(self, f: FnType<U, T>) -> FnType<U, R> {
        FnType::new(|u| self.call(f.apply(u)))
    }
}

impl<T: 'static, R: 'static> Clone for FnType<T, R> {
    fn clone(&self) -> Self {
        Self(self.0.dyn_clone())
    }
}

impl<T, R> Debug for FnType<T, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("FnType").field(&"...").finish()
    }
}

impl<T> Clone for Expression<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
        }
    }
}
