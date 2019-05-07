use std::rc::Weak;

mod lang;
mod executor;
use crate::lang::*;
use crate::executor::symbolic_execute;

fn main() {
    use crate::lang::Stmt as s;
    use crate::lang::NatExpr as e;
    use crate::lang::StmtKind::*;
    let sk = Block(vec![
        Assign(Variable("x"), e::konst(3)),
        Skip,
        Assign(Variable("x"), e::add(e::konst(3), e::konst(4))),
        Assign(Variable("x"), e::mul(e::var("x"), e::var("x"))),
        Block(vec![
            Assign(Variable("x"), e::mul(e::var("x"), e::var("x"))),
            Assign(Variable("y"), e::konst(2)),
            Assign(Variable("y"), e::add(e::mul(e::var("y"), e::konst(13)), e::konst(3))),
        ]),
        Fail,
    ]);
    let simple_fun = Function {
        params: vec![Variable("x"), Variable("y")],
        body: s::make(&sk, Weak::new(), Weak::new())
    };
    check_stmt_is_well_formed(&simple_fun.body);

    let fail_cases = symbolic_execute(&simple_fun);
    println!("{:#?}", fail_cases);
}
