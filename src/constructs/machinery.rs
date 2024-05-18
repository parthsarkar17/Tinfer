use std::{
    collections::{HashMap, HashSet},
    fmt,
};
pub enum Exp {
    Abs(char, Type, Box<Exp>),
    App(Box<Exp>, Box<Exp>),
    Plus(Box<Exp>, Box<Exp>),
    Var(char),
    Int(i32),
    _Unit,
}

impl Exp {
    pub fn generate_constraints(
        &self,
        delta: &InScope,
        gamma: &TypeContext,
    ) -> (InScope, Type, Constraints) {
        match self {
            Self::Abs(c, tau, exp) => {
                let new_in_scope = InScope::refresh(delta, tau);
                let (types_in_scope, subexp_type, subexp_cons) = exp
                    .as_ref()
                    .generate_constraints(&new_in_scope, &TypeContext::add_binding(gamma, c, tau));

                (
                    types_in_scope,
                    Type::Function(Box::new(tau.clone()), Box::new(subexp_type)),
                    subexp_cons,
                )
            }
            Self::App(e1, e2) => {
                let (s1, t1, c1) = e1.as_ref().generate_constraints(delta, gamma);
                let (s2, t2, c2) = e2.as_ref().generate_constraints(&s1, gamma);
                let fresh_var = Type::fresh(&s2);

                let application_constraints = Constraints {
                    constraints: HashSet::from([Constraint::Equality(
                        t1,
                        Type::Function(Box::new(t2), Box::new(fresh_var.clone())),
                    )]),
                };
                let c = Constraints::union(&Constraints::union(&c1, &c2), &application_constraints);

                (s2.refresh(&fresh_var), fresh_var, c)
            }
            Self::Plus(e1, e2) => {
                let (s1, t1, c1) = e1.as_ref().generate_constraints(delta, gamma);
                let (s2, t2, c2) = e2.as_ref().generate_constraints(&s1, gamma);
                let addition_constraints = Constraints {
                    constraints: HashSet::from([
                        Constraint::Equality(t1.clone(), Type::Int),
                        Constraint::Equality(t2.clone(), Type::Int),
                    ]),
                };

                (
                    s2,
                    Type::Int,
                    Constraints::union(&Constraints::union(&c1, &c2), &addition_constraints),
                )
            }
            Self::Var(c) => {
                let tau = gamma
                    .search_binding(c)
                    .expect("Type variable has no mapping in the context");
                (delta.clone(), tau.clone(), Constraints::new())
            }
            Self::Int(..) => (delta.clone(), Type::Int, Constraints::new()),
            Self::_Unit => (delta.clone(), Type::Unit, Constraints::new()),
        }
    }
}

#[derive(Eq, Hash, PartialEq, Clone)]
pub enum Type {
    Int,
    Unit,
    Abstract(u32),
    Function(Box<Type>, Box<Type>),
}
impl Type {
    fn fresh(abs_types: &InScope) -> Type {
        Self::Abstract(
            abs_types.abstract_type_indices.iter().fold(
                0,
                |acc, elt| {
                    if elt > &acc {
                        *elt
                    } else {
                        acc
                    }
                },
            ) + 1,
        )
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Unit => write!(f, "unit"),
            Type::Abstract(i) => write!(f, "t{i}"),
            Type::Function(b1, b2) => {
                let x = b1.as_ref();
                let y = b2.as_ref();
                write!(f, "{x} -> {y}")
            }
        }
    }
}

#[derive(Clone)]
pub struct InScope {
    abstract_type_indices: HashSet<u32>,
}

impl InScope {
    pub fn new() -> Self {
        InScope {
            abstract_type_indices: HashSet::new(),
        }
    }
    fn refresh(&self, tau: &Type) -> Self {
        if let Type::Abstract(i) = tau {
            let mut new_delta = self.abstract_type_indices.clone();
            let _ = new_delta.insert(*i);
            InScope {
                abstract_type_indices: new_delta,
            }
        } else {
            self.clone()
        }
    }
}
impl fmt::Display for InScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let x = self
            .abstract_type_indices
            .iter()
            .map(|elt| Type::Abstract(*elt))
            .fold("".to_string(), |elt, acc| format!("{acc}{elt} "));
        write!(f, "{x}")
    }
}

#[derive(Eq, Hash, PartialEq, Clone)]
pub enum Constraint {
    Equality(Type, Type),
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Equality(t1, t2) => write!(f, "{t1} = {t2}"),
        }
    }
}

pub struct TypeContext {
    context: HashMap<char, Type>,
}

impl TypeContext {
    pub fn new() -> Self {
        TypeContext {
            context: HashMap::new(),
        }
    }
    fn add_binding(&self, var: &char, var_type: &Type) -> Self {
        let mut cp = self.context.clone();
        let _ = cp.insert(*var, var_type.clone());
        TypeContext { context: cp }
    }

    fn search_binding(&self, var: &char) -> Option<&Type> {
        self.context.get(var)
    }
}

pub struct Constraints {
    constraints: HashSet<Constraint>,
}

impl Constraints {
    pub fn new() -> Self {
        Constraints {
            constraints: HashSet::new(),
        }
    }

    fn union(s1: &Constraints, s2: &Constraints) -> Self {
        let a = s1.constraints.clone();
        Constraints {
            constraints: a.union(&s2.constraints).fold(HashSet::new(), |acc, elt| {
                let mut cp = acc.clone();
                let _ = cp.insert(elt.clone());
                cp
            }),
        }
    }
}

impl fmt::Display for Constraints {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let constraints_string: String = self
            .constraints
            .iter()
            .fold("".to_string(), |acc: String, constraint| {
                format!("{acc}{constraint} ")
            });
        write!(f, "{constraints_string}")
    }
}
