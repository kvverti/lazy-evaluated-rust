use crate::{
    expression::{DataExpr, ExprCapable, Expression, FnType},
    Expr, Tup,
};

pub type Pair<A, B> = Tup!(A, B);

pub fn pair<A: ExprCapable, B: ExprCapable>() -> Expr!(A => B => Pair<A, B>) {
    Expression::new(FnType::new(|a| FnType::new(|b| (a, b))))
}

pub fn fst<A: ExprCapable, B: ExprCapable>() -> Expr!(Pair<A, B> => A) {
    Expression::new(FnType::new(|pair: Expression<Pair<A, B>>| {
        pair.eval().0.eval()
    }))
}

pub fn snd<A: ExprCapable, B: ExprCapable>() -> Expr!(Pair<A, B> => B) {
    Expression::new(FnType::new(|pair: Expression<Pair<A, B>>| {
        pair.eval().1.eval()
    }))
}

pub fn map_fst<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
) -> Expr!((A => B) => Pair<A, C> => Pair<B, C>) {
    Expression::new(FnType::new(|f| {
        FnType::new(|pair| {
            let (a, c) = DataExpr::destructure(pair);
            (f.apply(a), c)
        })
    }))
}

pub fn map_snd<A: ExprCapable, B: ExprCapable, C: ExprCapable>(
) -> Expr!((A => B) => Pair<C, A> => Pair<C, B>) {
    Expression::new(FnType::new(|f| {
        FnType::new(|pair| {
            let (c, a) = DataExpr::destructure(pair);
            (c, f.apply(a))
        })
    }))
}

pub fn map_pair<A: ExprCapable, B: ExprCapable, C: ExprCapable, D: ExprCapable>(
) -> Expr!((A => C) => (B => D) => Pair<A, B> => Pair<C, D>) {
    Expression::new(FnType::new(|f| {
        FnType::new(|g| {
            FnType::new(|pair| {
                let (a, b) = DataExpr::destructure(pair);
                (f.apply(a), g.apply(b))
            })
        })
    }))
}

pub fn map2_pair<
    A1: ExprCapable,
    A2: ExprCapable,
    B1: ExprCapable,
    B2: ExprCapable,
    C1: ExprCapable,
    C2: ExprCapable,
>() -> Expr!((A1 => B1 => C1) => (A2 => B2 => C2) => Pair<A1, A2> => Pair<B1, B2> => Pair<C1, C2>) {
    Expression::new(FnType::new(|f| {
        FnType::new(|g| {
            FnType::new(|pair_1| {
                FnType::new(|pair_2| {
                    let ((a1, b1), (a2, b2)) =
                        (DataExpr::destructure(pair_1), DataExpr::destructure(pair_2));
                    (f.apply(a1).apply(a2), g.apply(b1).apply(b2))
                })
            })
        })
    }))
}
