use std::rc::{Rc, Weak};
use std::boxed::Box;
use std::cell::RefCell;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Variable(pub &'static str);

#[derive(Debug)]
pub enum NatExpr {
    Add(Box<NatExpr>, Box<NatExpr>),
    Sub(Box<NatExpr>, Box<NatExpr>),
    Mul(Box<NatExpr>, Box<NatExpr>),
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
}


#[derive(Debug)]
pub enum StmtKind {
    Assign(Variable, NatExpr),
    Skip,
    Block(Vec<StmtKind>),
    Fail,
    Return(NatExpr),
}

/// Structured statements -- 'linked'.
#[derive(Debug)]
pub struct Stmt<'stmt> {
    pub(crate) parent: Weak<Stmt<'stmt>>,
    pub(crate) next_sib: Weak<Stmt<'stmt>>,
    pub(crate) children: RefCell<Vec<Rc<Stmt<'stmt>>>>,
    pub(crate) kind: &'stmt StmtKind,
}

pub fn next_stmt<'stmt>(s: &Rc<Stmt<'stmt>>) -> Rc<Stmt<'stmt>> {
    use StmtKind::*;
    match s.kind {
        Skip | Fail | Assign(..) | Return(..) => {
            let mut r = s.clone();
            while let None = r.next_sib.upgrade() {
                r = r.parent.upgrade().unwrap();
            }
            r.next_sib.upgrade().unwrap()
        }
        StmtKind::Block(..) =>
            s.children.borrow()[0].clone(),
    }
}

use StmtKind::*;
impl<'stmt> Stmt<'stmt> {
    pub fn make(kind: &'stmt StmtKind, parent: Weak<Stmt<'stmt>>, next_sib: Weak<Stmt<'stmt>>) -> Rc<Stmt<'stmt>> {
        match kind {
            Skip | Fail | Assign(..) | Return(..) =>
                Rc::new(Stmt {
                    parent, next_sib,
                    children: RefCell::new(Vec::new()),
                    kind
                }),
            Block(kinds) => {
                let mut s = Rc::new(Stmt {
                    parent, next_sib,
                    children: RefCell::new(Vec::new()),
                    kind
                });
                let children: Vec<_> = kinds.iter().rev().enumerate()
                    .scan(Weak::new(), |last, (i, kind)| {
                        let r = Stmt::make(kind, Rc::downgrade(&s), last.clone());
                        *last = Rc::downgrade(&r);
                        Some(r)
                    }).collect::<Vec<_>>().into_iter().rev().collect();
                *s.children.borrow_mut() = children;
                s
            },
        }
    }
}

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

#[derive(Debug)]
pub struct Function<'stmt> {
    pub params: Vec<Variable>,
    pub body: Rc<Stmt<'stmt>>,
}
