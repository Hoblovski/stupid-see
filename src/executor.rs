use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::cell::RefCell;

extern crate z3;

use crate::lang::*;
use crate::lang::Stmt as s;
use crate::lang::NatExpr as e;


type ConstraintSet<'ctx> = Vec<Rc<z3::Ast<'ctx>>>;

type StateSet<'ctx, 'stmt> = Vec<State<'ctx, 'stmt>>;

type FailCase = HashMap<&'static str, u32>;

type FailCaseSet = Vec<FailCase>;

type AbstractHeap<'ctx> = HashMap<&'static str, Rc<z3::Ast<'ctx>>>;
// possibly no builtin. use 3rd party

struct State<'ctx, 'stmt> {
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
        e::Const(v) =>
            Rc::new(z3::Ast::bv32_from_u64(ctx, *v as u64)),
        e::Var(Variable(name)) =>
            heap.get(name).expect("no such variable in current scope").clone(),
        e::Add(e1, e2) => {
            let e1 = evaluate(e1.as_ref(), heap, ctx);
            let e2 = evaluate(e2.as_ref(), heap, ctx);
            Rc::new(e1.bvadd(e2.as_ref()))
        },
        e::Sub(e1, e2) => {
            let e1 = evaluate(e1.as_ref(), heap, ctx);
            let e2 = evaluate(e2.as_ref(), heap, ctx);
            Rc::new(e1.bvsub(e2.as_ref()))
        },
        e::Mul(e1, e2) => {
            let e1 = evaluate(e1.as_ref(), heap, ctx);
            let e2 = evaluate(e2.as_ref(), heap, ctx);
            Rc::new(e1.bvmul(e2.as_ref()))
        },
        _ => unimplemented!(),
    }
}

fn get_fail_fail_cases<'ctx>(s: State<'ctx, '_>, ctx: &'ctx z3::Context) -> FailCaseSet {
    let solver = z3::Solver::new(ctx);
    if ! solver.check() { return vec![]; }
    let model = solver.get_model();

    let fc: FailCase = s.heap.iter()
        .map(|(&k, v)| (k, model.eval(&v).unwrap().as_u64().unwrap() as u32))
        .collect();

    vec![ fc ]
}

fn explore_state<'ctx, 'stmt>(s: State<'ctx, 'stmt>, ctx: &'ctx z3::Context) -> (StateSet<'ctx, 'stmt>, FailCaseSet) {
    use StmtKind::*;
    match &s.pc.kind {
        Block(..) | Skip => {
            (vec![State {
                heap: s.heap,
                conds: s.conds,
                pc: next_stmt(&s.pc),
            }], Vec::new())
        },
        Assign(Variable(name), rhs) => {
            let rhs_val = evaluate(rhs, &s.heap, ctx);
            let mut new_heap = s.heap.clone();
            *new_heap.get_mut(name)
                .expect("assigning to nonexistent variable")
                = rhs_val;
            // TODO: clone is too awkward
            (vec![State {
                heap: new_heap,
                conds: s.conds,
                pc: next_stmt(&s.pc),
            }], Vec::new())
        }
        Fail => {
            (Vec::new(), get_fail_fail_cases(s, ctx))
        },
        _ => unimplemented!(),
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
        println!("Selected State pc: \n{:#?}", cand.pc);
        let (mut new_states, mut new_fail_cases) = explore_state(cand, &ctx);
        for ns in new_states.iter() {
            println!("Explored State pc: \n{:#?}", ns.pc);
        }
        println!("==============================================================================");
        append_state(&mut states, new_states);
        fail_cases.append(&mut new_fail_cases);
    }

    fail_cases
}
