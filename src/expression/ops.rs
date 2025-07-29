//! Operator trait implementations for [super::Expression].

use crate::{fun, Expr};
use std::ops as stdops;

use super::{ExprCapable, Expression};

macro_rules! impl_binary_op {
    ($trait:ident, $func:ident) => {
        pub trait $trait<Rhs: ExprCapable>: ExprCapable {
            type Output: ExprCapable;

            fn $func() -> Expr!(Self => Rhs => Self::Output);
        }

        impl<T, Rhs> $trait<Rhs> for T
        where
            T: ExprCapable + stdops::$trait<Rhs>,
            Rhs: ExprCapable,
            T::Output: ExprCapable,
        {
            type Output = T::Output;

            fn $func() -> Expr!(Self => Rhs => Self::Output) {
                Expression::new(fun!(|a, b| stdops::$trait::$func(a.eval(), b.eval())))
            }
        }

        impl<T, Rhs> stdops::$trait<Expression<Rhs>> for Expression<T>
        where
            T: $trait<Rhs>,
            Rhs: ExprCapable,
        {
            type Output = Expression<T::Output>;

            fn $func(self, rhs: Expression<Rhs>) -> Self::Output {
                T::$func().apply(self).apply(rhs)
            }
        }

        impl<T, Rhs> stdops::$trait<Rhs> for Expression<T>
        where
            T: $trait<Rhs>,
            Rhs: ExprCapable,
        {
            type Output = Expression<T::Output>;

            fn $func(self, rhs: Rhs) -> Self::Output {
                T::$func().apply(self).apply_value(rhs)
            }
        }
    };
}

macro_rules! impl_binary_ops {
    ($([$($t:tt)*])*) => {
        $(impl_binary_op!($($t)*);)*
    }
}

macro_rules! impl_unary_op {
    ($trait:ident, $func:ident) => {
        pub trait $trait: ExprCapable {
            type Output: ExprCapable;

            fn $func() -> Expr!(Self => Self::Output);
        }

        impl<T> $trait for T
        where
            T: ExprCapable + stdops::$trait,
            T::Output: ExprCapable,
        {
            type Output = T::Output;

            fn $func() -> Expr!(Self => Self::Output) {
                Expression::new(fun!(|a| stdops::$trait::$func(a.eval())))
            }
        }

        impl<T> stdops::$trait for Expression<T>
        where
            T: $trait,
        {
            type Output = Expression<T::Output>;

            fn $func(self) -> Self::Output {
                T::$func().apply(self)
            }
        }
    };
}

impl_binary_ops! {
    [Add, add]
    [Sub, sub]
    [Mul, mul]
    [Div, div]
    [Rem, rem]
    [BitAnd, bitand]
    [BitOr, bitor]
    [Shl, shl]
    [Shr, shr]
}

impl_unary_op!(Not, not);
impl_unary_op!(Neg, neg);
