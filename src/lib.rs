use data::list::ConsList;
use expression::{ExprCapable, Expression, FnType};

pub mod expression;

pub mod control;
pub mod data;

/// An expression that never evaluates to a value. This is achieved
/// by immediately panicking when evaluated.
pub fn undefined<T: ExprCapable>() -> Expression<T> {
    Expression::lazy(|| panic!("tried to evaluate the bottom value"))
}

/// The identity function for the type `T`. It returns its input unchanged.
pub fn id<T: ExprCapable>() -> Expression<FnType<T, T>> {
    Expression::new(FnType::new(|x| x.eval()))
}

/// The constant function. It produces a function that ignores its input and evaluates to
/// the provided value.
pub fn constant<T: ExprCapable, R: ExprCapable>() -> Expression<FnType<R, FnType<T, R>>> {
    Expression::new(FnType::new(|c| FnType::new(|_| c.eval())))
}

/// Create an infinite list of the given value.
pub fn repeat<T: ExprCapable>() -> Expression<FnType<T, ConsList<T>>> {
    Expression::new(FnType::new(|x| {
        Expression::fix(FnType::new(|xs| ConsList::Cons { head: x, tail: xs })).eval()
    }))
}

/// The fixpoint combinator. It finds the least fixed point of the given function; that is,
/// the least-defined value `x` such that `f(x) = x`. This function will not terminate for
/// any function that unconditionally evaluates its argument.
pub fn fix<T: ExprCapable>() -> Expression<FnType<FnType<T, T>, T>> {
    Expression::new(FnType::new(|f: Expression<FnType<T, T>>| {
        let f = f.eval();
        Expression::fix(f).eval()
    }))
}

/// Composes two functions `f` and `g`, producing a function that evaluates `x` to `f(g(x))`.
pub fn compose<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
) -> Expression<FnType<FnType<B, C>, FnType<FnType<A, B>, FnType<A, C>>>> {
    Expression::new(FnType::new(|f| {
        FnType::new(|g| FnType::new(|x| f.apply(g.apply(x)).eval()))
    }))
}

pub fn curry<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
) -> Expression<FnType<FnType<(Expression<A>, Expression<B>), C>, FnType<A, FnType<B, C>>>> {
    Expression::new(FnType::new(|f| {
        FnType::new(|a| FnType::new(|b| f.apply(Expression::new((a, b))).eval()))
    }))
}

pub fn combine<A: ExprCapable, B: ExprCapable, C: ExprCapable, D: ExprCapable>() -> Expression<
    FnType<FnType<A, FnType<B, C>>, FnType<FnType<D, A>, FnType<FnType<D, B>, FnType<D, C>>>>,
> {
    Expression::new(FnType::new(|f| {
        FnType::new(|g| {
            FnType::new(|h| FnType::new(|x| f.apply(g.apply(x.clone())).apply(h.apply(x)).eval()))
        })
    }))
}

#[cfg(test)]
mod tests {
    use crate::expression::DataExpr;

    use super::*;

    #[test]
    fn repeat_fn() {
        let mut ls = repeat().apply(Expression::new(1));
        for _ in 1..100 {
            let ConsList::Cons { head, tail } = DataExpr::destructure(ls) else {
                panic!("List is not Cons");
            };
            assert_eq!(head.eval(), 1);
            ls = tail;
        }
    }

    #[test]
    fn fix_fn() {
        let factorial = fix().apply(Expression::new(FnType::new(|rec| {
            FnType::new(move |n| match n.eval() {
                0 | 1 => 1,
                n => n * rec.apply(Expression::new(n - 1)).eval(),
            })
        })));
        assert_eq!(factorial.clone().apply(Expression::new(0)).eval(), 1);
        assert_eq!(factorial.clone().apply(Expression::new(1)).eval(), 1);
        assert_eq!(factorial.clone().apply(Expression::new(2)).eval(), 2);
        assert_eq!(factorial.clone().apply(Expression::new(3)).eval(), 6);
        assert_eq!(factorial.clone().apply(Expression::new(4)).eval(), 24);
    }
}
