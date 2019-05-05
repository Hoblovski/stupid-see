use std::collections::HashMap;

extern crate z3;

use crate::lang::*;
use crate::lang::Stmt as s;
use crate::lang::NatExpr as e;

type ConstraintSet<'ctx> = Vec<&z3::Ast<'ctx>>
type StateSet = Vec<State>
type Report = z3::Model
type ReportSet = Vec<Report>

struct State<'ctx> {
    vars: HashMap<&'static str, z3::Ast<'ctx>>,
    conds: ConstraintSet
}

fn initial_states(f: &Function) -> StateSet {
    vec![State {
        vars: HashMap::new(),
        conds: Vec::new(),
    }]
}

fn select_state(s: &mut StateSet) -> State {
    s.pop().unwrap()
}

fn explore_state(s: State) -> (StateSet, ReportSet) {

}

fn append_state(s1: &mut State, mut s2: State) {
    s1.append(s2);
}

fn early_report(reports: ReportSet) {
    if reports.is_empty() return;
    println!("early report: fail!");
}

pub fn symbolic_execute(f: &Function) {
    let mut states = initial_states();
    while !states.is_empty() {
        let cand = select_state(&states);
        let (mut new_states, reports) = explore_state(cand);
        merge_state(&states, new_states);
        early_report(reports);
    }
}
