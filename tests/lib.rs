use std::rc::Weak;
use std::collections::HashMap;

extern crate stupid_see;
use stupid_see::lang::*;
use stupid_see::lang::StmtKind::*;
use stupid_see::lang::Stmt as s;
use stupid_see::lang::NatExpr as n;
use stupid_see::lang::BoolExpr as b;
use stupid_see::executor::symbolic_execute;

#[test]
fn test_straightline() {
    let sk = Block(vec![
        Assign(Variable("x"), n::konst(3)),
        Skip,
        Assign(Variable("x"), n::add(n::konst(3), n::konst(4))),
        Assign(Variable("x"), n::mul(n::var("x"), n::var("x"))),
        Block(vec![
            Assign(Variable("x"), n::mul(n::var("x"), n::var("x"))),
            Assign(Variable("y"), n::konst(2)),
            Assign(Variable("y"), n::add(n::mul(n::var("y"), n::konst(13)), n::konst(3))),
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

#[test]
fn test_if_then_else1() {
    let sk = Block(vec![
        IfThenElse(b::nateq(n::var("x"), n::konst(2371)),
            Box::new(Fail),
            Box::new(Skip)
        )
    ]);
    let simple_fun = Function {
        params: vec![Variable("x"), Variable("y"), Variable("z")],
        body: s::make(&sk, Weak::new(), Weak::new())
    };
    let fail_cases = symbolic_execute(&simple_fun);
    assert_eq!(fail_cases.len(), 1);
    let fail_case = &fail_cases[0];
    assert_eq!(fail_case.get("x").unwrap(), &2371);
}

#[test]
fn test_if_then_else_2() {
    // f(x, y) {
    //   if x > 0 && x < 10000 {
    //     if y > 0 {
    //       if x*x == y+6 {
    //         fail
    //       } else {
    //         skip
    //       }
    //     } else {
    //       skip
    //     }
    //   } else {
    //     skip
    //   }

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
    assert_eq!(fail_cases.len(), 1);
    let fail_case = &fail_cases[0];
    let x = *fail_case.get("x").unwrap();
    let y = *fail_case.get("y").unwrap();
    assert!(x > 0 && x < 10000);
    assert!(y > 0);
    assert_eq!(x*x, y+6);
}
