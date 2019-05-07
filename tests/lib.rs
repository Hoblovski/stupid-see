use std::rc::Weak;
use std::collections::HashMap;

extern crate stupid_see;
use stupid_see::lang::*;
use stupid_see::lang::StmtKind::*;
use stupid_see::lang::Stmt as s;
use stupid_see::lang::NatExpr as e;
use stupid_see::executor::symbolic_execute;

#[test]
fn test_straightline() {
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
    let fail_cases = symbolic_execute(&simple_fun);
    assert_eq!(fail_cases,
        vec![vec![("x", 2401u32), ("y", 29u32)].into_iter().collect::<HashMap<_, _>>()]
    );
}
