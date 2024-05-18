mod constructs;
use crate::constructs::machinery::{Exp, InScope, Type, TypeContext};

fn main() {
    // fun x y -> x + y
    let _exp1 = Exp::Abs(
        'x',
        Type::Abstract(0),
        Box::new(Exp::Abs(
            'y',
            Type::Abstract(1),
            Box::new(Exp::Plus(Box::new(Exp::Var('x')), Box::new(Exp::Var('y')))),
        )),
    );

    // fun x y -> x (y x) (5110)
    let _exp2 = Exp::Abs(
        'x',
        Type::Abstract(0),
        Box::new(Exp::Abs(
            'y',
            Type::Abstract(1),
            Box::new(Exp::App(
                Box::new(Exp::App(
                    Box::new(Exp::Var('x')),
                    Box::new(Exp::App(Box::new(Exp::Var('y')), Box::new(Exp::Var('x')))),
                )),
                Box::new(Exp::Int(5110)),
            )),
        )),
    );
    // fun f -> fun x -> 1 + (f x) : (int -> int) -> int -> int
    let _exp3 = Exp::Abs(
        'f',
        Type::Abstract(0),
        Box::new(Exp::Abs(
            'x',
            Type::Abstract(1),
            Box::new(Exp::Plus(
                Box::new(Exp::Int(1)),
                Box::new(Exp::App(Box::new(Exp::Var('f')), Box::new(Exp::Var('x')))),
            )),
        )),
    );

    let (_, t, c) = (&_exp2).generate_constraints(&InScope::new(), &TypeContext::new());
    println!("type: {t}");
    println!("constraints: {c}");
}
