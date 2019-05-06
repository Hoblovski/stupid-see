use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::cell::RefCell;

extern crate z3;

use crate::lang::*;
use crate::lang::Stmt as s;
use crate::lang::NatExpr as e;

type ConstraintSet<'ctx> = Vec<Rc<z3::Ast<'ctx>>>;
type StateSet<'ctx> = Vec<State<'ctx>>;
type Report<'ctx> = z3::Model<'ctx>;
type ReportSet<'ctx> = Vec<Report<'ctx>>;
type AbstractHeap<'ctx> = HashMap<&'static str, Rc<z3::Ast<'ctx>>>;

struct State<'ctx> {
    heap: AbstractHeap<'ctx>,
    conds: ConstraintSet<'ctx>,
    pc: Rc<Stmt>,
}

fn initial_states<'ctx>(f: &Function, ctx: &'ctx z3::Context) -> StateSet<'ctx> {
    let mut state: State = State {
        heap: HashMap::new(),
        conds: Vec::new(),
        pc: f.body.clone(),
    };
    for Variable(name) in f.params.iter() {
        let z3val = Rc::new(ctx.named_bitvector_const(name, 32));
        state.heap.entry(name).or_insert(z3val);
    }
    vec![state]
}

fn select_state<'ctx>(s: &mut StateSet<'ctx>) -> State<'ctx> {
    s.pop().unwrap()
}

fn evaluate<'ctx>(expr: &NatExpr, heap: &AbstractHeap<'ctx>, ctx: &'ctx z3::Context) -> Rc<z3::Ast<'ctx>> {
    match expr {
        e::Const(v) =>
            Rc::new(z3::Ast::bv32_from_u64(ctx, *v as u64)),
        _ => unimplemented!(),
    }
}

fn check_sat<'ctx>(s: State, ctx: &'ctx z3::Context) {
    let solver = z3::Solver::new(ctx);
    if solver.check() {
        println!("early report: failed!");
        let model = solver.get_model();
        for (k, v) in s.heap {
            let v = model.eval(&v).unwrap().as_u64().unwrap();
            println!("var for {} {}", k, v);
        }
    }
}

fn explore_state<'ctx>(s: State<'ctx>, ctx: &'ctx z3::Context) -> (StateSet<'ctx>, ReportSet<'ctx>) {
    match &s.pc.kind {
        StmtKind::Block => {
            (vec![State {
                heap: s.heap,
                conds: s.conds,
                pc: next_stmt(s.pc),
            }], Vec::new())
        },
        StmtKind::Skip => {
            (vec![State {
                heap: s.heap,
                conds: s.conds,
                pc: next_stmt(s.pc),
            }], Vec::new())
        },
        StmtKind::Assign { lhs: Variable(name), rhs: e } => {
            let rhs_val = evaluate(e, &s.heap, ctx);
            let mut new_heap = s.heap.clone();
            *new_heap.get_mut(name).expect("assigning to nonexistent variable") = rhs_val;
            // TODO: clone is too awkward
            (vec![State {
                heap: new_heap,
                conds: s.conds,
                pc: next_stmt(s.pc),
            }], Vec::new())
        }
        StmtKind::Fail => {
            check_sat(s, ctx);
            (Vec::new(), Vec::new())
        }
        _ => unimplemented!(),
    }
}

fn append_state<'ctx>(s1: &mut StateSet<'ctx>, mut s2: StateSet<'ctx>) {
    s1.append(&mut s2);
}

fn early_report(reports: ReportSet) {
    if reports.is_empty() { return; }
    println!("early report: fail!");
}

pub fn symbolic_execute(f: &Function) {
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let mut states = initial_states(f, &ctx);
    while !states.is_empty() {
        let cand = select_state(&mut states);
        println!("Selected State pc: \n{:#?}", cand.pc);
        let (mut new_states, reports) = explore_state(cand, &ctx);
        for ns in new_states.iter() {
            println!("Explored State pc: \n{:#?}", ns.pc);
        }
        println!("==============================================================================");
        append_state(&mut states, new_states);
        early_report(reports);
    }
}
