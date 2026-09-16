use crate::{
    Expr, Tup,
    expression::{DataExpr, Expression, FnType},
    funexp,
};

pub type Pair<A, B> = Tup!(A, B);

pub fn pair<A: Expr, B: Expr>() -> Expr!(A => B => Pair<A, B>) {
    funexp!(|a, b| (a, b))
}

pub fn fst<A: Expr, B: Expr>() -> Expr!(Pair<A, B> => A) {
    funexp!(|pair: Pair<A, B>| pair.eval().0.eval())
}

pub fn snd<A: Expr, B: Expr>() -> Expr!(Pair<A, B> => B) {
    Expression::new(FnType::new(|pair: Expression<Pair<A, B>>| {
        pair.eval().1.eval()
    }))
}

pub fn map_fst<A: Expr, B: Expr, C: Expr>()
-> Expr!((A => B) => Pair<A, C> => Pair<B, C>) {
    funexp!(|f, pair| {
        let (a, c) = DataExpr::destructure(pair);
        (f.apply(a), c)
    })
}

pub fn map_snd<A: Expr, B: Expr, C: Expr>()
-> Expr!((A => B) => Pair<C, A> => Pair<C, B>) {
    funexp!(|f, pair| {
        let (c, a) = DataExpr::destructure(pair);
        (c, f.apply(a))
    })
}

pub fn map_pair<A: Expr, B: Expr, C: Expr, D: Expr>()
-> Expr!((A => C) => (B => D) => Pair<A, B> => Pair<C, D>) {
    funexp!(|f, g, pair| {
        let (a, b) = DataExpr::destructure(pair);
        (f.apply(a), g.apply(b))
    })
}

pub fn map2_pair<
    A1: Expr,
    A2: Expr,
    B1: Expr,
    B2: Expr,
    C1: Expr,
    C2: Expr,
>() -> Expr!((A1 => B1 => C1) => (A2 => B2 => C2) => Pair<A1, A2> => Pair<B1, B2> => Pair<C1, C2>) {
    funexp!(|f, g, pair_1, pair_2| {
        let ((a1, b1), (a2, b2)) = (DataExpr::destructure(pair_1), DataExpr::destructure(pair_2));
        (f.apply(a1).apply(a2), g.apply(b1).apply(b2))
    })
}
