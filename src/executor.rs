use std::collections::HashMap;
use std::rc::{Rc};


extern crate z3;

use crate::lang::*;

use crate::lang::NatExpr as n;
use crate::lang::BoolExpr as b;


pub type ConstraintSet<'ctx> = Vec<Rc<z3::Ast<'ctx>>>;

pub type StateSet<'ctx, 'stmt> = Vec<State<'ctx, 'stmt>>;

pub type FailCase = HashMap<&'static str, u32>;

pub type FailCaseSet = Vec<FailCase>;

pub type AbstractHeap<'ctx> = HashMap<&'static str, Rc<z3::Ast<'ctx>>>;
// possibly no builtin. use 3rd party

#[derive(Clone)]
pub struct State<'ctx, 'stmt> {
    heap: AbstractHeap<'ctx>,
    conds: ConstraintSet<'ctx>,
    pc: Rc<Stmt<'stmt>>,
}

// lifetime can be elided like...
// fn initial_states(f: &Function, ctx: &'_ z3::Context) -> StateSet {
//
fn initial_states<'ctx, 'stmt>(f: &Function<'stmt>, ctx: &'ctx z3::Context) -> StateSet<'ctx, 'stmt> {
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

fn select_state<'ctx, 'stmt>(s: &mut StateSet<'ctx, 'stmt>) -> State<'ctx, 'stmt> {
    s.pop().unwrap()
}

fn evaluate<'ctx>(expr: &NatExpr, heap: &AbstractHeap<'ctx>, ctx: &'ctx z3::Context) -> Rc<z3::Ast<'ctx>> {
    match expr {
        n::Const(v) =>
            Rc::new(z3::Ast::bv32_from_u64(ctx, *v as u64)),
        n::Var(Variable(name)) =>
            heap.get(name).expect("no such variable in current scope").clone(),
        n::Add(e1, e2) => {
            let e1 = evaluate(e1.as_ref(), heap, ctx);
            let e2 = evaluate(e2.as_ref(), heap, ctx);
            Rc::new(e1.bvadd(e2.as_ref()))
        },
        n::Sub(e1, e2) => {
            let e1 = evaluate(e1.as_ref(), heap, ctx);
            let e2 = evaluate(e2.as_ref(), heap, ctx);
            Rc::new(e1.bvsub(e2.as_ref()))
        },
        n::Mul(e1, e2) => {
            let e1 = evaluate(e1.as_ref(), heap, ctx);
            let e2 = evaluate(e2.as_ref(), heap, ctx);
            Rc::new(e1.bvmul(e2.as_ref()))
        },
        n::Rem(e1, e2) => {
            let e1 = evaluate(e1.as_ref(), heap, ctx);
            let e2 = evaluate(e2.as_ref(), heap, ctx);
            Rc::new(e1.bvurem(e2.as_ref()))
        }
        _ => unimplemented!(),
    }
}

// TODO: awkward.
fn evaluate_bool<'ctx>(expr: &BoolExpr, heap: &AbstractHeap<'ctx>, ctx: &'ctx z3::Context) -> Rc<z3::Ast<'ctx>> {
    match expr {
        b::NatEq(e1, e2) => {
            let e1 = evaluate(e1, heap, ctx);
            let e2 = evaluate(e2, heap, ctx);
            Rc::new(e1._eq(&e2))
        },
        b::NatLe(e1, e2) => {
            let e1 = evaluate(e1, heap, ctx);
            let e2 = evaluate(e2, heap, ctx);
            Rc::new(e1.bvule(&e2))
        },
        b::NatLt(e1, e2) => {
            let e1 = evaluate(e1, heap, ctx);
            let e2 = evaluate(e2, heap, ctx);
            Rc::new(e1.bvult(&e2))
        },
        b::Neg(e1) => {
            let e1 = evaluate_bool(e1, heap, ctx);
            Rc::new(e1.not())
        }
        b::And(e1, e2) => {
            let e1 = evaluate_bool(e1, heap, ctx);
            let e2 = evaluate_bool(e2, heap, ctx);
            Rc::new(e1.and(&[&e2]))
        }
        b::Or(e1, e2) => {
            let e1 = evaluate_bool(e1, heap, ctx);
            let e2 = evaluate_bool(e2, heap, ctx);
            Rc::new(e1.or(&[&e2]))
        }
    }
}

fn get_fail_fail_cases<'ctx>(s: State<'ctx, '_>, ctx: &'ctx z3::Context, f: &Function) -> FailCaseSet {
    let solver = z3::Solver::new(ctx);
    for c in s.conds.iter() { solver.assert(c); }
    if ! solver.check() { return vec![]; }
    let model = solver.get_model();

    let fc: FailCase = f.params.iter()
        .map(|Variable(param_name)| (*param_name, model.eval(&ctx.named_bitvector_const(param_name, 32)).unwrap().as_u64().unwrap() as u32))
        .collect();

    vec![ fc ]
}

fn explore_state<'ctx, 'stmt>(mut s: State<'ctx, 'stmt>, ctx: &'ctx z3::Context, f: &Function<'stmt>)
    -> (StateSet<'ctx, 'stmt>, FailCaseSet)
{
    use StmtKind::*;
    use NextStmtResult::*;

    if let Fail = &s.pc.kind {
        return (Vec::new(), get_fail_fail_cases(s, ctx, f));
    }

    match next_stmt(&s.pc) {
        EndOfProgram =>
            (Vec::new(), Vec::new()),
        Branching(true_cl, false_cl) => {
            match &s.pc.kind {
                IfThenElse(cond, ..) => {
                    let mut s2 = s.clone();
                    let cond = evaluate_bool(cond, &s.heap, ctx);
                    s2.conds.push(Rc::new(cond.not()));
                    s2.pc = false_cl;
                    s.conds.push(cond);
                    s.pc = true_cl;
                    (vec![s, s2], Vec::new())
                },
                _ => panic!("unhandled explore_state"),
            }
        },
        Unique(next_pc) => {
            match &s.pc.kind {
                Block(..) | Skip => {
                    (vec![State { heap: s.heap, conds: s.conds, pc: next_pc, }],
                     Vec::new())
                },
                Assign(Variable(name), rhs) => {
                    let rhs_val = evaluate(rhs, &s.heap, ctx);
                    let mut new_heap = s.heap.clone();
                    *new_heap.get_mut(name)
                        .expect("assigning to nonexistent variable")
                        = rhs_val;
                    // TODO: clone is too awkward
                    (vec![State { heap: new_heap, conds: s.conds, pc: next_pc, }],
                     Vec::new())
                },
                DeclNatVar(Variable(name)) => {
                    let z3val = Rc::new(ctx.named_bitvector_const(name, 32));
                    s.heap.entry(name).or_insert(z3val);
                    (vec![State { pc: next_pc, ..s} ], Vec::new())
                }
                _ => panic!("unhandled explore_state"),
            }
        },
    }
}

fn append_state<'ctx, 'stmt>(s1: &mut StateSet<'ctx, 'stmt>, mut s2: StateSet<'ctx, 'stmt>) {
    s1.append(&mut s2);
}

pub fn symbolic_execute(f: &Function) -> FailCaseSet {
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let mut states = initial_states(f, &ctx);
    let mut fail_cases: FailCaseSet = Vec::new();

    while !states.is_empty() {
        let cand = select_state(&mut states);
        let (new_states, mut new_fail_cases) = explore_state(cand, &ctx, f);
        append_state(&mut states, new_states);
        fail_cases.append(&mut new_fail_cases);
    }

    fail_cases
}
