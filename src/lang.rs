/// The core languages executed by the SEE. Like LLVM IR for KLEE.
/// A simple structured imperivative language.
///
/// To symbolically execute other languages, translate them into this small core language.

use std::rc::{Rc, Weak};
use std::boxed::Box;
use std::cell::RefCell;

/// Wraps a variable.
/// Variables are unique w.r.t. their names, which is currently represented as &'static str's.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Variable(pub &'static str);

/// An expression with type u32.
/// Nat here is probably quite a misnomer.
/// Uses z3's bitvector, with 32-bit size and unsigned oeprations.
#[derive(Debug)]
pub enum NatExpr {
    Add(Box<NatExpr>, Box<NatExpr>),
    Sub(Box<NatExpr>, Box<NatExpr>),
    Mul(Box<NatExpr>, Box<NatExpr>),
    Rem(Box<NatExpr>, Box<NatExpr>),
    Const(u32),
    Var(Variable),
}

impl NatExpr {
    pub fn add(a: NatExpr, b: NatExpr) -> NatExpr {
        NatExpr::Add(Box::new(a), Box::new(b))
    }
    pub fn sub(a: NatExpr, b: NatExpr) -> NatExpr {
        NatExpr::Sub(Box::new(a), Box::new(b))
    }
    pub fn mul(a: NatExpr, b: NatExpr) -> NatExpr {
        NatExpr::Mul(Box::new(a), Box::new(b))
    }
    pub fn konst(v: u32) -> NatExpr {
        NatExpr::Const(v)
    }
    pub fn var(name: &'static str) -> NatExpr {
        NatExpr::Var(Variable(name))
    }
    pub fn rem(a: NatExpr, b: NatExpr) -> NatExpr {
        NatExpr::Rem(Box::new(a), Box::new(b))
    }
}

/// An expression with type bool.
#[derive(Debug)]
pub enum BoolExpr {
    NatEq(NatExpr, NatExpr),
    NatLe(NatExpr, NatExpr),
    NatLt(NatExpr, NatExpr),
    Neg(Box<BoolExpr>),
    And(Box<BoolExpr>, Box<BoolExpr>),
    Or(Box<BoolExpr>, Box<BoolExpr>),
}

impl BoolExpr {
    pub fn nateq(a: NatExpr, b: NatExpr) -> BoolExpr {
        BoolExpr::NatEq(a, b)
    }
    pub fn natge(a: NatExpr, b: NatExpr) -> BoolExpr {
        BoolExpr::NatLe(b, a)
    }
    pub fn natgt(a: NatExpr, b: NatExpr) -> BoolExpr {
        BoolExpr::NatLt(b, a)
    }
    pub fn natle(a: NatExpr, b: NatExpr) -> BoolExpr {
        BoolExpr::NatLe(a, b)
    }
    pub fn natlt(a: NatExpr, b: NatExpr) -> BoolExpr {
        BoolExpr::NatLt(a, b)
    }
    pub fn neg(a: BoolExpr) -> BoolExpr {
        BoolExpr::Neg(Box::new(a))
    }
    pub fn and(a: BoolExpr, b: BoolExpr) -> BoolExpr {
        BoolExpr::And(Box::new(a), Box::new(b))
    }
    pub fn or(a: BoolExpr, b: BoolExpr) -> BoolExpr {
        BoolExpr::Or(Box::new(a), Box::new(b))
    }
}

/// The program statements.
///
/// Should look like the AST definition.
#[derive(Debug)]
pub enum StmtKind {
    Assign(Variable, NatExpr),
    Skip,
    Block(Vec<StmtKind>),
    Fail,
    Return(NatExpr),
    IfThenElse(BoolExpr, Box<StmtKind>, Box<StmtKind>),
    DeclNatVar(Variable),
    While(BoolExpr, Box<StmtKind>),
}

/// Structured statements -- 'linked'.
///
/// Link the statements so we can easily traverse them while executing.
#[derive(Debug)]
pub struct Stmt<'stmt> {
    pub(crate) parent: Weak<Stmt<'stmt>>,
    pub(crate) next_sib: Weak<Stmt<'stmt>>,
    pub(crate) children: RefCell<Vec<Rc<Stmt<'stmt>>>>,
    pub(crate) kind: &'stmt StmtKind,
}

/// What statement should be executed after the current one.
// TODO: definition more rigid with CFG
pub enum NextStmtResult<'stmt> {
    EndOfProgram,
    Branching(Rc<Stmt<'stmt>>, Rc<Stmt<'stmt>>), // true, false
    Unique(Rc<Stmt<'stmt>>),
}

pub fn next_stmt<'stmt>(s: &Rc<Stmt<'stmt>>) -> NextStmtResult<'stmt> {
    use StmtKind::*;
    use NextStmtResult as R;

    let next_sequential = || { // mimic lazy
        let mut r = s.clone();
        while let None = r.next_sib.upgrade() {
            match r.parent.upgrade() {
                None => return None,
                Some(x) => r = x,
            }
        }
        r.next_sib.upgrade()
    };

    match s.kind {
        Skip | Fail | Assign(..) | Return(..) | DeclNatVar(..) => {
            if let Some(x) = next_sequential() { R::Unique(x) }
            else { R::EndOfProgram }
        },
        Block(..) =>
            R::Unique(s.children.borrow()[0].clone()),
        IfThenElse(..) => {
            let c = s.children.borrow();
            R::Branching(c[0].clone(), c[1].clone())
        },
        While(..) => {
            R::Branching(s.children.borrow()[0].clone(), next_sequential().unwrap())
            // XXX: this forbids while's as last statements
        }
    }
}

use StmtKind::*;
impl<'stmt> Stmt<'stmt> {
    pub fn make(kind: &'stmt StmtKind, parent: Weak<Stmt<'stmt>>, next_sib: Weak<Stmt<'stmt>>) -> Rc<Stmt<'stmt>> {
        match kind {
            Skip | Fail | Assign(..) | Return(..) | DeclNatVar(..) =>
                Rc::new(Stmt {
                    parent, next_sib,
                    children: RefCell::new(Vec::new()),
                    kind
                }),
            Block(kinds) => {
                let s = Rc::new(Stmt {
                    parent, next_sib,
                    children: RefCell::new(Vec::new()),
                    kind
                });
                let children: Vec<_> = kinds.iter().rev()
                    .scan(Weak::new(), |last, kind| {
                        let r = Stmt::make(kind, Rc::downgrade(&s), last.clone());
                        *last = Rc::downgrade(&r);
                        Some(r)
                    }).collect::<Vec<_>>().into_iter().rev().collect();
                *s.children.borrow_mut() = children;
                s
            },
            IfThenElse(_, s1, s2) => {
                let s = Rc::new(Stmt {
                    parent, next_sib,
                    children: RefCell::new(Vec::new()),
                    kind
                });
                let s1 = Stmt::make(s1, Rc::downgrade(&s), Weak::new());
                let s2 = Stmt::make(s2, Rc::downgrade(&s), Weak::new());
                *s.children.borrow_mut() = vec![s1, s2];
                s
            },
            While(_, s1) => {
                let s = Rc::new(Stmt {
                    parent, next_sib,
                    children: RefCell::new(Vec::new()),
                    kind
                });
                // Note: while-sub . next_sib is while itself!
                // XXX this is due to CFG semantics. Other parts are inconsisent.
                let s1 = Stmt::make(s1, Rc::downgrade(&s), Rc::downgrade(&s));
                *s.children.borrow_mut() = vec![s1];
                s
            }
        }
    }
}

/// A function type. Like AST definition.
#[derive(Debug)]
pub struct Function<'stmt> {
    pub params: Vec<Variable>,
    pub body: Rc<Stmt<'stmt>>,
}



#[cfg(test)]
mod tests {
    use super::*;

    fn check_nontop_stmt_is_well_formed(s: &Rc<Stmt>) {
        let children = s.children.borrow();
        for (i, c) in children.iter().enumerate() {
            assert!(Rc::ptr_eq(&c.parent.upgrade().unwrap(), &s));
            check_nontop_stmt_is_well_formed(c);
            if i+1 == children.len() {
                assert!(c.next_sib.upgrade().is_none());
            } else {
                assert!(Rc::ptr_eq(
                        &c.next_sib.upgrade().unwrap(), &children[i+1]));
            }
        }
    }

    pub fn check_stmt_is_well_formed(top: &Rc<Stmt>) {
        assert!(top.parent.upgrade().is_none());
        assert!(top.next_sib.upgrade().is_none());
        check_nontop_stmt_is_well_formed(top);
        println!("check_stmt_is_well_formed passed")
    }

    /// Ensure Stmt::make returns well-formed Stmt,
    /// so we do not get stuck while executing.
    #[test]
    fn well_formedness() {
        use super::NatExpr as e;
        use super::StmtKind::*;
        let sk = Block(vec![
                       Assign(Variable("x"), e::konst(3)),
                       Skip,
                       Assign(Variable("x"), e::add(e::konst(3), e::konst(4))),
                       Assign(Variable("x"), e::mul(e::var("x"), e::var("x"))),
                       Block(vec![
                             Assign(Variable("x"), e::mul(e::var("x"), e::var("x"))),
                             Assign(Variable("y"), e::konst(2)),
                             Block(vec![
                                   Assign(Variable("x"), e::mul(e::var("x"), e::var("x"))),
                                   Assign(Variable("y"), e::konst(2)),
                                   Block(vec![
                                         Assign(Variable("x"), e::mul(e::var("x"), e::var("x"))),
                                         Assign(Variable("y"), e::konst(2)),
                                         Assign(Variable("y"), e::add(e::mul(e::var("y"), e::konst(13)), e::konst(3))),
                                   ]),
                                   Assign(Variable("y"), e::add(e::mul(e::var("y"), e::konst(13)), e::konst(3))),
                             ]),
                             Assign(Variable("y"), e::add(e::mul(e::var("y"), e::konst(13)), e::konst(3))),
                       ]),
                       Fail,
                       ]);
        let simple_fun = Function {
            params: vec![Variable("x"), Variable("y")],
            body: Stmt::make(&sk, Weak::new(), Weak::new())
        };
        check_stmt_is_well_formed(&simple_fun.body);
    }
}
