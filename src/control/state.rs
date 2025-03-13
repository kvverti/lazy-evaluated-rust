use crate::expression::{ExprCapable, Expression, FnType};

use super::TypeCtor;

pub struct StateT<S: ExprCapable, T: TypeCtor, A: ExprCapable> {
    run: Expression<FnType<S, T::Apply<(Expression<S>, Expression<A>)>>>,
}