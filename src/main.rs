use std::rc::Weak;

mod lang;
mod executor;
use crate::lang::*;
use crate::lang::Stmt as s;
use crate::lang::NatExpr as e;
use crate::executor::symbolic_execute;

fn main() {
    use crate::lang::StmtKind::*;
    let sk = Block(vec![
        Assign(Variable("x"), e::Const(3)),
        Skip,
        Assign(Variable("x"), e::Const(4)),
        Fail,
    ]);
    let simple_fun = Function {
        params: vec![Variable("x")],
        body: s::make(&sk, Weak::new(), Weak::new())
    };
    check_stmt_is_well_formed(&simple_fun.body);

    symbolic_execute(&simple_fun);
}
