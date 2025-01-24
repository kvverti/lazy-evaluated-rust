use crate::expression::{DataExpr, ExprCapable, Expression, FnType};

use super::{Associative, Monoid};

/// A lazy cons list.
#[derive(Debug, Clone)]
pub enum ConsList<T> {
    Nil,
    Cons {
        head: Expression<T>,
        tail: Expression<Self>,
    },
}

impl<T: ExprCapable> ConsList<T> {
    pub fn concat() -> Expression<FnType<Self, FnType<Self, Self>>> {
        Expression::fix(FnType::new(|rec| {
            FnType::new(|list_a| {
                FnType::new(|list_b| match DataExpr::destructure(list_a) {
                    ConsList::Nil => list_b.eval(),
                    ConsList::Cons { head, tail } => ConsList::Cons {
                        head,
                        tail: rec.apply(tail).apply(list_b),
                    },
                })
            })
        }))
    }
}

impl<T: ExprCapable> ExprCapable for ConsList<T> {}

impl<T: ExprCapable> DataExpr for ConsList<T> {
    fn destructure(v: Expression<Self>) -> Self {
        v.eval()
    }
}

impl<T: ExprCapable> Associative for ConsList<T> {
    fn append() -> Expression<FnType<Self, FnType<Self, Self>>> {
        Self::concat()
    }
}

impl<T: ExprCapable> Monoid for ConsList<T> {
    fn empty() -> Expression<Self> {
        Expression::new(Self::Nil)
    }
}
