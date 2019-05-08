use std::rc::Weak;


use stupid_see::lang::*;
use stupid_see::lang::StmtKind::*;
use stupid_see::lang::Stmt as s;
use stupid_see::lang::NatExpr as n;
use stupid_see::lang::BoolExpr as b;
use stupid_see::executor::symbolic_execute;

fn main() {
    let sk = Block(vec![
        IfThenElse(b::and(b::natgt(n::var("x"), n::konst(0)), b::natlt(n::var("x"), n::konst(10000))),
            Box::new(IfThenElse(b::natgt(n::var("y"), n::konst(0)),
                Box::new(IfThenElse(b::nateq(n::mul(n::var("x"), n::var("x")), n::add(n::konst(6), n::var("y"))),
                    Box::new(Fail),
                    Box::new(Skip))),
                Box::new(Skip))),
            Box::new(Skip)),
    ]);
    let simple_fun = Function {
        params: vec![Variable("x"), Variable("y")],
        body: s::make(&sk, Weak::new(), Weak::new())
    };
    let fail_cases = symbolic_execute(&simple_fun);
    println!("fail cases:\n{:#?}", fail_cases);
}
