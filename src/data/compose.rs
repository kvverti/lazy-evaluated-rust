use crate::{
    control::{Applicative, Functor, TypeCtor},
    expression::{DataExpr, ExprCapable, Expression, FnType},
};

use super::Newtype;

/// A composition of two type constructors.
#[derive(Debug, Clone)]
pub struct Compose<F: TypeCtor, G: TypeCtor, A: ExprCapable>(Expression<F::Apply<G::Apply<A>>>);

impl<F: TypeCtor, G: TypeCtor, A: ExprCapable> ExprCapable for Compose<F, G, A> {}
impl<F: TypeCtor, G: TypeCtor, A: ExprCapable> DataExpr for Compose<F, G, A> {
    fn destructure(v: Expression<Self>) -> Self {
        Self(Expression::lazy(|| v.eval().0.eval()))
    }
}

impl<F: TypeCtor, G: TypeCtor, A: ExprCapable> Newtype for Compose<F, G, A> {
    type Inner = F::Apply<G::Apply<A>>;

    fn lift(v: Expression<Self::Inner>) -> Self {
        Self(v)
    }

    fn unlift(v: Expression<Self>) -> Self::Inner {
        v.eval().0.eval()
    }
}

impl<F: TypeCtor, G: TypeCtor> TypeCtor for Compose<F, G, ()> {
    type Apply<T: ExprCapable> = Compose<F, G, T>;
}

impl<F: Functor, G: Functor> Functor for Compose<F, G, ()> {
    fn map<A: ExprCapable, B: ExprCapable>(
    ) -> Expression<FnType<FnType<A, B>, FnType<Self::Apply<A>, Self::Apply<B>>>> {
        Expression::new(FnType::new(|f| {
            FnType::new(|compose| {
                let Compose(compose) = DataExpr::destructure(compose);
                Compose(F::map().apply(G::map().apply(f)).apply(compose))
            })
        }))
    }
}

impl<F: Applicative, G: Applicative> Applicative for Compose<F, G, ()> {
    fn pure<A: ExprCapable>() -> Expression<FnType<A, Self::Apply<A>>> {
        Expression::new(FnType::new(|a| {
            Compose(F::pure().apply(G::pure().apply(a)))
        }))
    }

    fn map2<A: ExprCapable, B: ExprCapable, C: ExprCapable>() -> Expression<
        FnType<
            FnType<A, FnType<B, C>>,
            FnType<Self::Apply<A>, FnType<Self::Apply<B>, Self::Apply<C>>>,
        >,
    > {
        Expression::new(FnType::new(|f| {
            FnType::new(|comp1| {
                FnType::new(|comp2| {
                    let (Compose(comp1), Compose(comp2)) =
                        (DataExpr::destructure(comp1), DataExpr::destructure(comp2));
                    Compose(
                        F::map2()
                            .apply(G::map2().apply(f))
                            .apply(comp1)
                            .apply(comp2),
                    )
                })
            })
        }))
    }
}
