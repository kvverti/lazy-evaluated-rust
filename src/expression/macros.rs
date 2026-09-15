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

#[doc(hidden)]
#[macro_export]
macro_rules! __letrec_expr {
    (; $value:expr) => {
        $value
    };
    ($([$var:ident ; $($ty:ty)? ; $init:expr ; $($pat:pat)?])+ ; $value:expr) => {{
        $crate::__create_letrec_struct!($($var)+);
        #[allow(unused_variables)]
        let LetRecVars { $($var,)* }: LetRecVars<$($crate::__opt_ty!($($ty)?),)*> = $crate::expression::DataExpr::destructure(
            $crate::expression::Expression::fix($crate::fun!(|rec| {
                let LetRecVars { $($var,)* } = $crate::expression::DataExpr::destructure(rec);
                $($(
                    let $pat = $var;
                )?)+
                LetRecVars {
                    $($var: $init,)*
                }
            }))
        );
        $($(
            let $pat = $var;
        )?)+
        $value
    }};
}

/// Defines a collection of mutually recursive bindings and constructs an expression using them.
///
/// ## Usage
/// ```
/// use lazy::expression::Expression;
/// use lazy::{letrec, fun};
///
/// let x = letrec!({
///     let even = Expression::new(fun!(|n| match n.eval() {
///         0 => true,
///         n => odd.apply_value(n - 1).eval()
///     }));
///     let odd = Expression::new(fun!(|n| match n.eval() {
///         0 => false,
///         n => even.apply_value(n - 1).eval(),
///     }));
///     even.apply_value(10u32).eval()
/// });
/// assert_eq!(x, true);
/// ```
#[macro_export]
macro_rules! letrec {
    ({let $var:ident $(: $ty:ty)? = $init:expr; $($rest:tt)*} $($bindings:tt)*) => {
        $crate::letrec!({$($rest)*} [$var ; $($ty)? ; $init ;] $($bindings)*)
    };
    ({let pat!($pat:pat) $(: $ty:ty)? = $init:expr; $($rest:tt)*} $($bindings:tt)*) => {
        $crate::letrec!({$($rest)*} [var ; $($ty)? ; $init ; $pat] $($bindings)*)
    };
    {{$value:expr} $($bindings:tt)*} => {
        $crate::__letrec_expr!($($bindings)* ; $value)
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __fntype {
    ($($move:ident)? [$arg:tt $($ty:ty)?] ; -> $ret:ty $body:block) => {
        $crate::expression::FnType::new($($move)? |$arg: $crate::expression::Expression<$crate::__opt_ty!($($ty)?)>| -> $ret { $body })
    };
    ($($move:ident)? [$arg:tt $($ty:ty)?] ; $body:expr) => {
        $crate::expression::FnType::new($($move)? |$arg: $crate::expression::Expression<$crate::__opt_ty!($($ty)?)>| $body)
    };
    ($($move:ident)? [$arg:tt $($ty:ty)?] $($tail:tt)+) => {
        $crate::expression::FnType::new($($move)? |$arg: $crate::expression::Expression<$crate::__opt_ty!($($ty)?)>| $crate::__fntype!($($move)? $($tail)+))
    };
}

/// Create a function from a closure.
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
        $crate::__fntype!($($move)? $([$args $($ty)?])* ; $($body)*)
    };
}

/// Create a function expression from a closure.
#[macro_export]
macro_rules! funexp {
    ($($t:tt)*) => {
        $crate::expression::Expression::new($crate::fun!($($t)*))
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __ado_expr {
    ($app:path ; $value:expr) => {
        <$app as $crate::control::Applicative>::pure()
            .apply($value)
    };
    ($app:path $([$binding:ident ; $($ty:ty)? ; $($pat:pat)?])+ ; $value:expr) => {
        <$app as $crate::control::Applicative>::pure()
            .apply_value($crate::fun!(
                |$($binding: $crate::__opt_ty!($($ty)?)),*| {
                    $($(
                        let $pat = $crate::expression::DataExpr::destructure($binding);
                    )?)*
                    $crate::expression::Expression::eval($value)
                }
            ))
    };
    ($app:path [$init:expr] $($rest:tt)+) => {
        <$app as $crate::control::Applicative>::ap()
            .apply($crate::__ado_expr!($app $($rest)+))
            .apply($init)
    };
}

/// Applicative do-notation. This is similar to monadic do-notation ([mdo!]), but the applicative
/// bindings may not refer to one another and the result expression must be pure. In return, applicative
/// do-notation can be used with any applicative type constructor.
#[doc(hidden)]
#[macro_export]
macro_rules! ado {
    ({use $app:path; let $var:ident $(: $ty:ty)? = $init:expr; $($rest:tt)+} $($bindings:tt)*) => {
        $crate::ado!({use $app; $($rest)+} [$init] $($bindings)* [$var ; $($ty)? ;])
    };
    ({use $app:path; let _ = $init:expr; $($rest:tt)+} $($bindings:tt)*) => {
        $crate::ado!({use $app; $($rest)+} [$init] $($bindings)* [blank ; ; _])
    };
    ({use $app:path; let pat!($pat:pat) $(: $ty:ty)? = $init:expr; $($rest:tt)+} $($bindings:tt)*) => {
        $crate::ado!({use $app; $($rest)+} [$init] $($bindings)* [arg ; $($ty)? ; $pat])
    };
    ({use $app:path; $init:expr; $($rest:tt)+} $($bindings:tt)*) => {
        $crate::ado!({use $app; $($rest)+} [$init] $($bindings)* [blank ; ; _])
    };
    ({use $app:path; return $value:expr $(;)?} $($bindings:tt)*) => {
        $crate::__ado_expr!($app $($bindings)* ; $value)
    };
}

/// Monadic do-notation.
#[macro_export]
macro_rules! mdo {
    ({use $monad:path; let $var:ident $(: $ty:ty)? = $init:expr; $($rest:tt)+}) => {
        <$monad as $crate::control::Monad>::bind()
            .apply_value($crate::fun! {
                |$var $(: $ty)?| { $crate::mdo!({use $monad; $($rest)+}).eval() }
            })
            .apply($init)
    };
    ({use $monad:path; let _ = $init:expr; $($rest:tt)+}) => {
        <$monad as $crate::control::Monad>::sequence()
            .apply({ $crate::mdo!({use $monad; $($rest)+}) })
            .apply($init)
    };
    ({use $monad:path; let pat!($pat:pat) $(: $ty:ty)? = $init:expr; $($rest:tt)+}) => {
        <$monad as $crate::control::Monad>::bind()
            .apply_value($crate::fun! {
                |input $(: $ty)?| {
                    let $pat = $crate::expression::DataExpr::destructure(input);
                    { $crate::mdo!({use $monad; $($rest)+}) }.eval()
                }
            })
            .apply($init)
    };
    ({use $monad:path; $init:expr; $($rest:tt)+}) => {
        <$monad as $crate::control::Monad>::sequence()
            .apply({ $crate::mdo!({use $monad; $($rest)+}) })
            .apply($init)
    };
    ({use $monad:path; return $init:expr $(;)?}) => {
        <$monad as $crate::control::Applicative>::pure()
            .apply($init)
    };
    ({use $monad:path; $init:expr;}) => {
        <$monad as $crate::control::Functor>::map()
            .apply($crate::function::constant().apply_value(()))
            .apply($init)
    };
    ({use $monad:path; $init:expr}) => {
        $init
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __dorec_expr {
    ($monad:path ; $value:expr) => {
        $value
    };
    ($monad:path $([$var:ident ; $($ty:ty)? ; $init:expr ; $($pat:pat)?])+ ; $value:expr) => {{
        $crate::__create_letrec_struct!($($var)+);
        $crate::mdo!({
            use $monad;
            let pat!(LetRecVars { $(#[allow(unused_variables)] $var,)* }): LetRecVars<$($crate::__opt_ty!($($ty)?),)*> = (
                <$monad as $crate::control::MonadFix>::mfix($crate::fun!(|rec| {
                    let LetRecVars { $(#[allow(unused_variables)] $var,)* } = $crate::expression::DataExpr::destructure(rec);
                    $($(
                        let $pat = $crate::expression::DataExpr::destructure($var);
                    )?)+
                    $crate::ado!({
                        use $monad;
                        $(let $var $(: $ty)? = $init;)*
                        return $crate::expression::Expression::new(LetRecVars {
                            $($var,)*
                        })
                    }).eval()
                }))
            );
            {
                $($(
                    let $pat = $crate::expression::DataExpr::destructure($var);
                )?)+
                $value
            }
        })
    }};
}

/// Recursive monadic do-notation
#[macro_export]
macro_rules! dorec {
    ({use $monad:path; let $var:ident $(: $ty:ty)? = $init:expr; $($rest:tt)+} $($bindings:tt)*) => {
        $crate::dorec!({use $monad; $($rest)*} $($bindings)* [$var ; $($ty)? ; $init ;])
    };
    ({use $monad:path; let _ = $init:expr; $($rest:tt)+} ; $($bindings:tt)*) => {
        $crate::dorec!({use $monad; $($rest)*} $($bindings)* [blank ; ; $init ; _])
    };
    ({use $monad:path; let pat!($pat:pat) $(: $ty:ty)? = $init:expr; $($rest:tt)+} $($bindings:tt)*) => {
        $crate::dorec!({use $monad; $($rest)*} $($bindings)* [var ; $($ty)? ; $init ; $pat])
    };
    ({use $monad:path; $init:expr; $($rest:tt)+} $($bindings:tt)*) => {
        $crate::dorec!({use $monad; $($rest)*} $($bindings)* [blank ; ; $init ; _])
    };
    ({use $monad:path; return $init:expr $(;)?} $($bindings:tt)*) => {
        $crate::__dorec_expr!(
            $monad $($bindings)* ;
            <$monad as $crate::control::Applicative>::pure()
                .apply($init)
        )
    };
    ({use $monad:path; $init:expr;} $($bindings:tt)*) => {
        $crate::__dorec_expr!(
            $monad $($bindings)* ;
            <$monad as $crate::control::Functor>::map()
                .apply($crate::function::constant().apply_value(()))
                .apply($init)
        )
    };
    ({use $monad:path; $init:expr} $($bindings:tt)*) => {
        $crate::__dorec_expr!(
            $monad $($bindings)* ;
            $init
        )
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn arithmetic() {
        let x = letrec!({
            let fact: crate::ExprType!(u128 => u128) = crate::funexp!(|n| match n.eval() {
                0 | 1 => 1,
                n => n * fact.apply_value(n - 1).eval(),
            });
            fact.apply_value(5).eval()
        });
        assert_eq!(x, 120);
    }
}
