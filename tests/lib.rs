use std::rc::Weak;
use std::collections::HashMap;

extern crate stupid_see;
use stupid_see::lang::*;
use stupid_see::lang::StmtKind::*;
use stupid_see::lang::Stmt as s;
use stupid_see::lang::NatExpr as n;
use stupid_see::lang::BoolExpr as b;
use stupid_see::executor::*;

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
    assert_eq!(fail_cases.len(), 1);
}

#[test]
fn test_if_then_else_1() {
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
    let x = *fail_cases[0].get("x").unwrap();
    let y = *fail_cases[0].get("y").unwrap();
    assert!(x > 0 && x < 10000);
    assert!(y > 0);
    assert_eq!(x*x, y+6);
}

// any_all D1 D2
//  forall f :: D2 . exists x :: D1 . P(f, x)
fn any_all<T>(v: &Vec<T>, f: Vec<Box<Fn(&T)->bool>>) -> bool
{
    f.into_iter().all(|f| {
        v.iter().any(f)
    })
}

#[test]
fn test_if_then_else_3() {
    let sk = Block(vec![
        IfThenElse(b::and(b::natgt(n::var("x"), n::konst(0)), b::natlt(n::var("x"), n::konst(10000))),
            Box::new(Fail),
            Box::new(Skip)),
        IfThenElse(b::and(b::natgt(n::var("y"), n::konst(0)), b::natlt(n::var("y"), n::konst(10000))),
            Box::new(Fail),
            Box::new(Skip)),
        Assign(Variable("x"), n::add(n::var("x"), n::konst(10000))),
        IfThenElse(b::and(b::natgt(n::var("x"), n::konst(0)), b::natlt(n::var("x"), n::konst(10000))),
            Box::new(Fail),
            Box::new(Skip)),
    ]);
    let simple_fun = Function {
        params: vec![Variable("x"), Variable("y")],
        body: s::make(&sk, Weak::new(), Weak::new())
    };
    let fail_cases = symbolic_execute(&simple_fun);
    assert!(fail_cases.len() == 3);
    println!("{:#?}", fail_cases);

    assert!(any_all(&fail_cases, vec![
        Box::new(|fail_case| {
            let x = *fail_case.get("x").unwrap();
            let y = *fail_case.get("y").unwrap();
            x > 0 && x < 10000
        }),
        Box::new(|fail_case| {
            let x = *fail_case.get("x").unwrap();
            let y = *fail_case.get("y").unwrap();
            (x == 0 || x >= 10000) && y > 0 && y < 10000
        }),
        Box::new(|fail_case| {
            let x = *fail_case.get("x").unwrap();
            let y = *fail_case.get("y").unwrap();
            (x == 0 || x >= 10000) && (y == 0 || y >= 10000) &&
            x.wrapping_add(10000) < 10000
        }),
    ]));
}

#[test]
fn test_newvar_quadratic_equation() {
    // f(x)
    //   if x < 100
    //     var y
    //     y := x * x
    //     if y == 2*x + 238
    //       fail
    //     else skip
    //   else skip
    let sk = Block(vec![
        IfThenElse(b::natle(n::var("x"), n::konst(100)),
            Box::new(Block(vec![
                DeclNatVar(Variable("y")),
                Assign(Variable("y"), n::mul(n::var("x"), n::var("x"))),
                IfThenElse(b::nateq(n::var("y"), n::add(n::mul(n::konst(3), n::var("x")), n::konst(238))),
                    Box::new(Fail),
                    Box::new(Skip)),])),
            Box::new(Skip)),
    ]);
    let simple_fun = Function {
        params: vec![Variable("x")],
        body: s::make(&sk, Weak::new(), Weak::new())
    };
    let fail_cases = symbolic_execute(&simple_fun);
    assert!(fail_cases.len() == 1);
    assert!(any_all(&fail_cases, vec![
        Box::new(|fail_case| {
            let x = *fail_case.get("x").unwrap();
            x == 17
        }),
    ]));
}

#[test]
fn test_remainder() {
    let sk = Block(vec![
        IfThenElse(b::and(b::natge(n::var("x"), n::konst(100)), b::natle(n::var("x"), n::konst(500))),
            Box::new(IfThenElse(b::nateq(n::rem(n::var("x"), n::konst(67)), n::konst(32)),
                Box::new(Fail),
                Box::new(Skip))),
            Box::new(Skip)),
    ]);
    let simple_fun = Function {
        params: vec![Variable("x")],
        body: s::make(&sk, Weak::new(), Weak::new())
    };
    let fail_cases = symbolic_execute(&simple_fun);
    assert!(fail_cases.len() == 1);
    assert!(any_all(&fail_cases, vec![
        Box::new(|fail_case| {
            let x = *fail_case.get("x").unwrap();
            x >= 100 && x <= 500 && x % 67 == 32
        }),
    ]));
}

#[test]
fn test_while_1() {
    let sk = IfThenElse(b::natle(n::var("n"), n::konst(100)),
        Box::new(Block(vec![
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
                Box::new(Skip),
                Box::new(Fail))
        ])),
        Box::new(Skip));

    let simple_fun = Function {
        params: vec![Variable("n")],
        body: s::make(&sk, Weak::new(), Weak::new())
    };
    let fail_cases = symbolic_execute(&simple_fun);

    assert!(fail_cases.is_empty());
}

#[test]
fn test_while_2() {
    let sk = IfThenElse(b::natle(n::var("n"), n::konst(100)),
        Box::new(Block(vec![
            DeclNatVar(Variable("x")),
            Assign(Variable("x"), n::konst(0)),
            DeclNatVar(Variable("y")),
            Assign(Variable("y"), n::var("n")),
            While(b::natgt(n::var("y"), n::konst(0)),
                Box::new(Block(vec![
                    Assign(Variable("x"), n::add(n::var("x"), n::var("n"))),
                    Assign(Variable("y"), n::sub(n::var("y"), n::konst(1))),
                ]))),
            IfThenElse(b::nateq(n::var("x"), n::rem(n::mul(n::var("n"), n::var("n")), n::konst(9376))),
                Box::new(Skip),
                Box::new(Fail))
        ])),
        Box::new(Skip));

    let simple_fun = Function {
        params: vec![Variable("n")],
        body: s::make(&sk, Weak::new(), Weak::new())
    };
    let fail_cases = symbolic_execute(&simple_fun);

    assert!(fail_cases.len() == 4);
    assert!(any_all(&fail_cases, vec![
        Box::new(|fail_case| {
            let n = *fail_case.get("n").unwrap();
            n == 97
        }),
        Box::new(|fail_case| {
            let n = *fail_case.get("n").unwrap();
            n == 98
        }),
        Box::new(|fail_case| {
            let n = *fail_case.get("n").unwrap();
            n == 99
        }),
        Box::new(|fail_case| {
            let n = *fail_case.get("n").unwrap();
            n == 100
        }),
    ]));
}

#[test]
fn test_while_3() {
    let sk = Block(vec![
        Assign(Variable("x"), n::rem(n::var("x"), n::konst(3))),
        // x := x % 3
        While(b::natle(n::var("x"), n::konst(10)),
            Box::new(Assign(Variable("x"), n::add(n::var("x"), n::konst(7))))),
        // while x <= 10 do x := x + 7 end while
        IfThenElse(b::nateq(n::var("x"), n::konst(12)),
            Box::new(Fail), Box::new(Skip)),
        // assert x != 12
    ]);
    let simple_fun = Function {
        params: vec![Variable("x")],
        body: s::make(&sk, Weak::new(), Weak::new())
    };
    let fail_cases = symbolic_execute(&simple_fun);
    assert!(fail_cases.is_empty());
}

#[test]
fn test_while_4() {
    let sk = Block(vec![
        Assign(Variable("x"), n::rem(n::var("x"), n::konst(3))),
        // x := x % 3
        While(b::natle(n::var("x"), n::konst(10)),
            Box::new(Assign(Variable("x"), n::add(n::var("x"), n::konst(3))))),
        // while x <= 10 do x := x + 7 end while
        IfThenElse(b::nateq(n::var("x"), n::konst(12)),
            Box::new(Fail), Box::new(Skip)),
        // assert x != 12
    ]);
    let simple_fun = Function {
        params: vec![Variable("x")],
        body: s::make(&sk, Weak::new(), Weak::new())
    };
    let fail_cases = symbolic_execute(&simple_fun);
    assert!(fail_cases.len() == 1);
    assert!(any_all(&fail_cases, vec![
        Box::new(|fail_case| {
            let x = *fail_case.get("x").unwrap();
            x % 3 == 0
        }),
    ]));
}
