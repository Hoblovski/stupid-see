mod lang;
use crate::lang::*;
use crate::lang::Stmt as s;
use crate::lang::NatExpr as e;

fn main() {
    let simple_fun = Function {
        params: vec![Variable("x")],
        body: s::mkBlock(vec![
            s::mkAssign(Variable("x"), e::Const(1)),
            s::mkSkip(),
            s::mkAssign(Variable("x"), e::add(e::var("x"), e::Const(1))),
            s::mkFail(),
        ])
    };
    check_stmt_is_well_formed(&simple_fun.body);
    println!("{:#?}", simple_fun);
}
