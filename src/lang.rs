use std::rc::{Rc, Weak};
use std::cell::RefCell;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Variable(pub &'static str);

#[derive(Debug)]
pub enum NatExpr {
    Add(Rc<NatExpr>, Rc<NatExpr>),
    Sub(Rc<NatExpr>, Rc<NatExpr>),
    Mul(Rc<NatExpr>, Rc<NatExpr>),
    Const(u32),
    Var(Variable),
}

impl NatExpr {
    pub fn add(a: NatExpr, b: NatExpr) -> NatExpr {
        NatExpr::Add(Rc::new(a), Rc::new(b))
    }

    pub fn var(name: &'static str) -> NatExpr {
        NatExpr::Var(Variable(name))
    }
}


#[derive(Debug)]
enum StmtKind {
    Assign { lhs: Variable, rhs: NatExpr },
    Skip,
    Block,
    Fail,
    Return { e: NatExpr },
}

#[derive(Debug)]
/// Structured statements -- 'linked'.
pub struct Stmt {
    parent: RefCell<Weak<Stmt>>,   // if upgrade is null ...
    next_sib: RefCell<Weak<Stmt>>, // if upgrade is null ...
    children: RefCell<Vec<Rc<Stmt>>>,
    kind: StmtKind,
}

use StmtKind::*;
impl Stmt {
    pub fn mkAssign(lhs: Variable, rhs: NatExpr) -> Rc<Stmt> {
        Rc::new(Stmt {
            parent: RefCell::new(Weak::new()),
            next_sib: RefCell::new(Weak::new()),
            children: RefCell::new(vec![]),
            kind: Assign { lhs, rhs }
        })
    }

    pub fn mkSkip() -> Rc<Stmt> {
        Rc::new(Stmt {
            parent: RefCell::new(Weak::new()),
            next_sib: RefCell::new(Weak::new()),
            children: RefCell::new(vec![]),
            kind: Skip,
        })
    }

    pub fn mkBlock(v: Vec<Rc<Stmt>>) -> Rc<Stmt> {
        let s = Rc::new(Stmt {
            parent: RefCell::new(Weak::new()),
            next_sib: RefCell::new(Weak::new()),
            children: RefCell::new(v),
            kind: Block
        });
        {let mut children = s.children.borrow_mut();
        for (i, c) in children.iter().enumerate() {
            *c.parent.borrow_mut() = Rc::downgrade(&s);
            if i+1 == children.len() { break; }
            *c.next_sib.borrow_mut() = Rc::downgrade(&children[i+1]);
        }}
        s
    }

    pub fn mkFail() -> Rc<Stmt> {
        Rc::new(Stmt {
            parent: RefCell::new(Weak::new()),
            next_sib: RefCell::new(Weak::new()),
            children: RefCell::new(vec![]),
            kind: Fail,
        })
    }

    pub fn mkReturn(e: NatExpr) -> Rc<Stmt> {
        Rc::new(Stmt {
            parent: RefCell::new(Weak::new()),
            next_sib: RefCell::new(Weak::new()),
            children: RefCell::new(vec![]),
            kind: Return { e },
        })
    }
}

fn check_nontop_stmt_is_well_formed(s: &Rc<Stmt>) {
    let children = s.children.borrow();
    for (i, c) in children.iter().enumerate() {
        assert!(Rc::ptr_eq(
            &c.parent.borrow().upgrade().unwrap(), &s));
        check_nontop_stmt_is_well_formed(c);
        if i+1 == children.len() {
            assert!(c.next_sib.borrow().upgrade().is_none());
        } else {
            assert!(Rc::ptr_eq(
                &c.next_sib.borrow().upgrade().unwrap(),
                &children[i+1]));
        }
    }
}

pub fn check_stmt_is_well_formed(top: &Rc<Stmt>) {
    assert!(top.parent.borrow().upgrade().is_none());
    assert!(top.next_sib.borrow().upgrade().is_none());
    check_nontop_stmt_is_well_formed(top);
    println!("check_stmt_is_well_formed passed")
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<Variable>,
    pub body: Rc<Stmt>,
}
