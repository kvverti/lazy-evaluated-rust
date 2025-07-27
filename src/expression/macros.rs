#[doc(hidden)]
#[macro_export]
macro_rules! __create_letrec_struct {
    ($($fields:ident)*) => {
        #[allow(non_camel_case_types)]
        #[derive(Debug, Clone)]
        struct LetRecVars<$($fields,)*>
        {
            $($fields: $crate::expression::Expression<$fields>,)*
        }
        #[allow(non_camel_case_types)]
        impl<$($fields: $crate::expression::ExprCapable,)*> $crate::expression::ExprCapable for LetRecVars<$($fields,)*> {}
        #[allow(non_camel_case_types)]
        impl<$($fields: $crate::expression::ExprCapable,)*> $crate::expression::DataExpr for LetRecVars<$($fields,)*> {
            fn destructure(v: $crate::expression::Expression<Self>) -> Self {
                Self {$(
                    $fields: $crate::expression::Expression::lazy({
                        let v = v.clone(); move || v.eval_ref().$fields.eval_ref().clone()
                    }),
                )*}
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __opt_ty {
    ($ty:ty) => {
        $ty
    };
    () => {
        _
    };
}

/// Defines a collection of mutually recursive bindings and constructs an expression using them.
/// 
/// ## Usage
/// ```
/// use lazy::expression::Expression;
/// use lazy::{letrec, fun};
/// 
/// let x = letrec! {
///     even = Expression::new(fun!(|n| match n.eval_ref() {
///         0 => true,
///         _ => odd.apply(n.map(|n| n - 1)).eval(),
///     }));
///     odd = Expression::new(fun!(|n| match n.eval_ref() {
///         0 => false,
///         _ => even.apply(n.map(|n| n - 1)).eval(),
///     }));
///     => even.apply(Expression::new(10u32)).eval()
/// };
/// assert_eq!(x, true);
/// ```
#[macro_export]
macro_rules! letrec {
    ($($var:ident $(: $ty:ty)? = $init:expr;)+ => $value:expr) => {
        {
            $crate::__create_letrec_struct!($($var)*);
            #[allow(unused)]
            let LetRecVars { $($var,)* }: LetRecVars<$($crate::__opt_ty!($($ty)?),)*> = $crate::expression::DataExpr::destructure(
                $crate::expression::Expression::fix($crate::expression::FnType::new(|rec| {
                    let LetRecVars { $($var,)* } = $crate::expression::DataExpr::destructure(rec);
                    LetRecVars {
                        $($var: $init,)*
                    }
                }))
            );
            $value
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __fntype {
    ($($move:ident)? [$arg:tt $($ty:ty)?] -> $ret:ty $body:block) => {
        $crate::expression::FnType::new($($move)? |$arg $(: $crate::expression::Expression<$ty>)?| -> $ret { $body })
    };
    ($($move:ident)? [$arg:tt $($ty:ty)?] $body:expr) => {
        $crate::expression::FnType::new($($move)? |$arg $(: $crate::expression::Expression<$ty>)?| $body)
    };
    ($($move:ident)? [$arg:tt $($ty:ty)?] $($tail:tt)+) => {
        $crate::expression::FnType::new($($move)? |$arg $(: $crate::expression::Expression<$ty>)?| $crate::__fntype!($($move)? $($tail)+))
    };
}

/// Create a function expression from a closure.
///
/// ## Usage
/// ```
/// use lazy::expression::Expression;
/// use lazy::fun;
///
/// let add = fun!(|a, b| a.eval() + b.eval());
/// assert_eq!(add.apply(Expression::new(3)).apply(Expression::new(5)).eval(), 8);
/// ```
#[macro_export]
macro_rules! fun {
    ($($move:ident)? | $($args:tt $(: $ty:ty)?),* $(,)? | $($body:tt)*) => {
        $crate::__fntype!($($move)? $([$args $($ty)?])* $($body)*)
    };
}

#[cfg(test)]
mod tests {
    use crate::expression::Expression;

    #[test]
    fn arithmetic() {
        let x = letrec! {
            fact: crate::ExprType!(u128 => u128) = Expression::new(crate::fun!(|n| {
                let n = n.eval();
                if n < 2 {
                    1
                } else {
                    n * fact.apply(Expression::new(n - 1)).eval()
                }
            }));
            => fact.apply(Expression::new(5)).eval()
        };
        assert_eq!(x, 120);
    }
}
