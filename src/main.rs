use std::rc::Weak;


use stupid_see::lang::*;
use stupid_see::lang::StmtKind::*;
use stupid_see::lang::Stmt as s;
use stupid_see::lang::NatExpr as n;
use stupid_see::lang::BoolExpr as b;
use stupid_see::executor::symbolic_execute;

fn main() {
    let sk = Block(vec![
        DeclNatVar(Variable("x")),
        Assign(Variable("x"), n::konst(0)),
        DeclNatVar(Variable("y")),
        Assign(Variable("y"), n::var("n")),
        While(b::natgt(n::var("y"), n::konst(0)),
            Box::new(Block(vec![
                Assign(Variable("x"), n::add(n::var("x"), n::var("n"))),
                Assign(Variable("y"), n::sub(n::var("y"), n::konst(1))),
            ]))),
        IfThenElse(b::nateq(n::var("x"), n::mul(n::var("n"), n::var("n"))),
            Box::new(Fail),
            Box::new(Skip))
    ]);

    let simple_fun = Function {
        params: vec![Variable("n")],
        body: s::make(&sk, Weak::new(), Weak::new())
    };
    let fail_cases = symbolic_execute(&simple_fun);
    println!("fail cases:\n{:#?}", fail_cases);
}
