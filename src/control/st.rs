//! The ST monad, which represents effectful computations.

use std::{cell::RefCell, marker::PhantomData, rc::Rc};

use crate::{Expr, expression::Expression, funexp};

/// A trait that erases all effectful computations.
/// Cloning such a computation yields a computation that will perform the same effectful
/// algorithm, but may not produce the same result.
trait ImpureThunk<A>: FnOnce() -> A + 'static {
    fn dyn_clone(&self) -> Box<dyn ImpureThunk<A>>;
}

impl<F, A> ImpureThunk<A> for F
where
    F: FnOnce() -> A + Clone + 'static,
{
    fn dyn_clone(&self) -> Box<dyn ImpureThunk<A>> {
        Box::new(self.clone())
    }
}

/// The ST type contains an impure (effectful) computation in such a way that it can be
/// transformed with pure code. This type provides access to strict mutable cells, and
/// a way to extract pure values from the computation with [`run_st!`].
pub struct ST<S: Expr, A: Expr> {
    // the "thread" or "local frame" this ST executes in
    thread: PhantomData<S>,
    // performs some effects, then produces a (pure) computation
    thunk: Box<dyn ImpureThunk<Expression<A>>>,
}

impl<S: Expr, A: Expr> Clone for ST<S, A> {
    fn clone(&self) -> Self {
        Self {
            thread: PhantomData,
            thunk: self.thunk.dyn_clone(),
        }
    }
}

impl<S: Expr, A: Expr> Expr for ST<S, A> {}

impl<S: Expr, A: Expr> ST<S, A> {
    /// Constructs a new `ST` from a potentially impure computation.
    pub(crate) fn from_impure(f: impl FnOnce() -> Expression<A> + Clone + 'static) -> Self {
        Self {
            thread: PhantomData,
            thunk: Box::new(f),
        }
    }

    /// Run the impure computation within this `ST` and extract the value.
    /// It is recommended to use [`run_st!`] instead of this function.
    pub fn impure_run_st(self) -> Expression<A> {
        (self.thunk)()
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! run_st {
    ($st:expr) => {
        ({
            // this struct is a unique type by virtue of being defined within
            // its own scope. We explicitly provide this type to impure_run_st.
            #[derive(Clone)]
            struct Thread;
            impl $crate::Expr for Thread {}
            $crate::control::st::ST::<Thread, _>::impure_run_st
        })($crate::expression::Expression::eval($st))
    };
}

/// Runs an `ST` computation to completion and extracts the resultant pure value. This macro
/// is structured in such a way that there are no observable effects from running the effectful
/// computation.
pub use run_st;

/// A strict mutable cell holding a value of type `A` and associated with an [`ST`] with type `S`.
/// This cell is "strict" in the sense that operations on the cell (loads, stores, updates) evaluate
/// both the cell and the value to be stored within.
#[derive(Debug, Clone)]
pub struct STRef<S: Expr, A: Expr> {
    thread: PhantomData<S>,
    // note: all expressions are unsync anyway, revisit this if multithreading is ever a thing
    cell: Rc<RefCell<A>>,
}

impl<S: Expr, A: Expr> Expr for STRef<S, A> {}

impl<S: Expr, A: Expr> STRef<S, A> {
    pub fn new() -> Expr!(A => ST<S, Self>) {
        funexp!(|a| ST::from_impure(|| Expression::new(Self {
            thread: PhantomData,
            cell: Rc::new(RefCell::new(a.eval()))
        })))
    }

    pub fn get() -> Expr!(Self => ST<S, A>) {
        funexp!(|rf: Self| ST::from_impure(|| Expression::new(rf.eval().cell.borrow().clone())))
    }

    pub fn set() -> Expr!(Self => A => ST<S, ()>) {
        funexp!(|rf: Self, a| ST::from_impure(|| Expression::new(
            *rf.eval().cell.borrow_mut() = a.eval()
        )))
    }

    pub fn update() -> Expr!(Self => (A => A) => ST<S, ()>) {
        funexp!(|rf: Self, f| ST::from_impure(|| {
            let rf = rf.eval();
            let v = rf.cell.borrow().clone();
            *rf.cell.borrow_mut() = f.apply_value(v).eval();
            Expression::new(())
        }))
    }
}

pub mod inst {
    use std::marker::PhantomData;

    use crate::{
        Expr, ExprType,
        control::{Applicative, Functor, Monad, MonadFix, TypeCtor},
        expression::Expression,
        fun, funexp,
    };

    use super::ST as STTy;

    #[derive(Debug, Clone)]
    pub struct ST<S: Expr>(PhantomData<S>);

    impl<S: Expr> TypeCtor for ST<S> {
        type Apply<T: Expr> = super::ST<S, T>;
    }

    impl<S: Expr> Functor for ST<S> {
        fn map<A: Expr, B: Expr>() -> Expr!((A => B) => Self::Apply<A> => Self::Apply<B>) {
            funexp!(
                |f, st: Self::Apply<_>| STTy::from_impure(|| f.apply(st.eval().impure_run_st()))
            )
        }
    }

    impl<S: Expr> Applicative for ST<S> {
        fn pure<A: Expr>() -> Expr!(A => Self::Apply<A>) {
            funexp!(|a| STTy::from_impure(|| a))
        }

        fn map2<A: Expr, B: Expr, C: Expr>()
        -> Expr!((A => B => C) => Self::Apply<A> => Self::Apply<B> => Self::Apply<C>) {
            funexp!(
                |f, sta: Self::Apply<_>, stb: Self::Apply<_>| STTy::from_impure(|| f
                    .apply(sta.eval().impure_run_st())
                    .apply(stb.eval().impure_run_st()))
            )
        }

        fn ap<A: Expr, B: Expr>()
        -> Expr!(Self::Apply<ExprType!(A => B)> => Self::Apply<A> => Self::Apply<B>) {
            funexp!(
                |stf: Self::Apply<_>, sta: Self::Apply<_>| STTy::from_impure(|| stf
                    .eval()
                    .impure_run_st()
                    .apply(sta.eval().impure_run_st()))
            )
        }
    }

    impl<S: Expr> Monad for ST<S> {
        fn bind<A: Expr, B: Expr>()
        -> Expr!((A => Self::Apply<B>) => Self::Apply<A> => Self::Apply<B>) {
            // bind f = join . (map f)
            funexp!(|f| Self::join().compose(Self::map().apply(f)).eval())
        }

        fn join<A: Expr>() -> Expr!(Self::Apply<Self::Apply<A>> => Self::Apply<A>) {
            funexp!(
                |ststa: Self::Apply<Self::Apply<_>>| STTy::from_impure(|| ststa
                    .eval()
                    .impure_run_st()
                    .eval()
                    .impure_run_st())
            )
        }

        fn sequence<A: Expr, B: Expr>() -> Expr!(Self::Apply<B> => Self::Apply<A> => Self::Apply<B>)
        {
            funexp!(
                |stb: Self::Apply<_>, sta: Self::Apply<_>| STTy::from_impure(|| {
                    _ = sta.eval().impure_run_st();
                    stb.eval().impure_run_st()
                })
            )
        }
    }

    impl<S: Expr> MonadFix for ST<S> {
        fn mfix<A: Expr>(f: ExprType!(A => Self::Apply<A>)) -> Expr!(Self::Apply<A>) {
            Expression::new(STTy::from_impure(|| {
                Expression::fix(fun!(|a: A| f.apply(a).eval().impure_run_st().eval()))
            }))
        }
    }

    #[cfg(test)]
    mod tests {
        use std::cell::Cell;

        use crate::{ado, control::st::STRef, dorec, mdo};

        use super::*;

        #[test]
        fn single() {
            let count = Box::leak(Box::new(Cell::new(0)));
            let add1 = Expression::new(STTy::from_impure(|| {
                count.update(|c| c + 1);
                Expression::new(count.get())
            }));
            let result = run_st!(add1);
            assert_eq!(result.eval(), 1);
            assert_eq!(count.get(), 1);
        }

        #[test]
        fn multiple() {
            let count = Box::leak(Box::new(Cell::new(0)));
            let add1 = Expression::new(STTy::from_impure(|| {
                count.update(|c| c + 1);
                Expression::new(count.get())
            }));
            let add1_2 = add1.clone();
            let result = run_st!(mdo!({
                use ST<_>;
                let x = add1;
                let y = add1_2;
                return x + y;
            }));
            assert_eq!(result.eval(), 3);
            assert_eq!(count.get(), 2);
        }

        #[test]
        fn mfix_int() {
            let count = Box::leak(Box::new(Cell::new(0)));
            let add1 = Expression::new(STTy::from_impure(|| {
                count.update(|c| c + 1);
                Expression::new(count.get())
            }));
            let result = run_st!(dorec!({
                use ST<_>;
                let y = ST::pure().apply(x);
                let x = add1;
                return y;
            }));
            assert_eq!(result.eval(), 1);
            assert_eq!(count.get(), 1);
        }

        #[test]
        fn stref() {
            let result = run_st!(mdo!({
                use ST<_>;
                let x = STRef::new().apply_value(2);
                // cloning in mdo is impossible...
                ado!({
                    use ST<_>;
                    STRef::set().apply(x.clone()).apply_value(4);
                    STRef::update().apply(x).apply_value(fun!(|x| x.eval() * 3));
                    return Expression::new(());
                });
                STRef::get().apply(x.clone())
            }));
            assert_eq!(result.eval(), 12);
        }
    }
}
