use crate::{
    control::TypeCtor,
    expression::{ExprCapable, Expression, FnType},
    id,
};

pub mod constant;
pub mod list;
pub mod maybe;

pub trait Associative: ExprCapable {
    fn append() -> Expression<FnType<Self, FnType<Self, Self>>>;
}

pub trait Monoid: Associative {
    fn empty() -> Expression<Self>;
}

pub trait Foldable: TypeCtor {
    fn foldr<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, FnType<B, B>>, FnType<B, FnType<Self::Apply<A>, B>>>>;

    fn foldl_strict<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<B, FnType<A, B>>, FnType<B, FnType<Self::Apply<A>, B>>>> {
        Expression::new(FnType::new(|f| {
            FnType::new(|b| {
                FnType::new(|this| {
                    Self::foldr()
                        .apply(Expression::new(FnType::new(|a| {
                            FnType::new(|g| {
                                FnType::new(|b| g.apply_strict(f.apply(b).apply(a)).eval())
                            })
                        })))
                        .apply(id())
                        .apply(this)
                        .apply(b)
                        .eval()
                })
            })
        }))
    }
}
