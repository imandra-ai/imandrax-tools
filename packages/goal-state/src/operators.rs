//! Ported from imandrax-vscode/src/goal-state/imandrax_operators.ts
//!
//! OCaml-style operator precedence and associativity (operator_info,
//! operator_info_of_term, needs_parentheses, short_id)

use imandrax_api::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Notation {
    None,
    Infix,
    Prefix,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Associativity {
    None,
    Left,
    Right,
}

#[derive(Debug, Clone)]
pub struct OperatorInfo {
    pub name: String,
    pub notation: Notation,
    pub associativity: Associativity,
    pub precedence: f64,
}

impl OperatorInfo {
    fn new(name: &str, notation: Notation, associativity: Associativity, precedence: f64) -> Self {
        Self {
            name: name.to_string(),
            notation,
            associativity,
            precedence,
        }
    }
}

/// Get the short form of an identifier (strip after last `/`).
pub fn short_id(id: &str) -> &str {
    match id.rfind('/') {
        Some(0) | None => id,
        Some(pos) => &id[..pos],
    }
}

pub fn operator_info(op: &str, more_than_one_arg: bool) -> OperatorInfo {
    // See also https://ocaml.org/manual/5.3/expr.html#ss:precedence-and-associativity

    // prefix-symbol
    if (op.starts_with('!') || op.starts_with('?') || op.starts_with('~'))
        && op.len() > 1
        && op != "~-"
        && op != "~-."
    {
        return OperatorInfo::new(op, Notation::Prefix, Associativity::None, 20.0);
    }
    if op.starts_with('#') {
        return OperatorInfo::new(op, Notation::Infix, Associativity::Left, 18.0);
    }
    if op == "assert" || op == "lazy" {
        return OperatorInfo::new(op, Notation::Prefix, Associativity::Left, 17.0);
    }
    // - -. (prefix)
    if (op == "-" || op == "-." || op == "~-" || op == "~-.") && !more_than_one_arg {
        return OperatorInfo::new(op, Notation::Prefix, Associativity::None, 16.0);
    }
    // **… lsl lsr asr
    if op.starts_with("**") || op == "lsl" || op == "lsr" || op == "asr" {
        return OperatorInfo::new(op, Notation::Infix, Associativity::Right, 15.0);
    }
    // *… /… %… mod land lor lxor
    if op.starts_with('*')
        || op.starts_with('/')
        || op.starts_with('%')
        || op == "mod"
        || op == "land"
        || op == "lor"
        || op == "lxor"
    {
        return OperatorInfo::new(op, Notation::Infix, Associativity::Left, 14.0);
    }
    // +… -…
    if op.starts_with('+') || op.starts_with('-') {
        return OperatorInfo::new(op, Notation::Infix, Associativity::Left, 13.0);
    }
    // ::
    if op == "::" {
        return OperatorInfo::new(op, Notation::Infix, Associativity::Right, 12.0);
    }
    // @… ^…
    if op.starts_with('@') || op.starts_with('^') {
        return OperatorInfo::new(op, Notation::Infix, Associativity::Right, 11.0);
    }
    // =… <… >… |… &… $… !=
    if op.starts_with('=')
        || op.starts_with('<')
        || op.starts_with('>')
        || (op.starts_with('|') && op != "||")
        || (op.starts_with('&') && op != "&" && op != "&&")
        || op.starts_with('$')
        || op == "!="
    {
        return OperatorInfo::new(op, Notation::Infix, Associativity::Left, 10.0);
    }
    // & &&
    if op == "&" || op == "&&" {
        return OperatorInfo::new(op, Notation::Infix, Associativity::Right, 9.0);
    }
    // or ||
    if op == "or" || op == "||" {
        return OperatorInfo::new(op, Notation::Infix, Associativity::Right, 8.0);
    }
    // ,
    if op == "," {
        return OperatorInfo::new(op, Notation::None, Associativity::None, 7.0);
    }
    // <- :=
    if op == "<-" || op == ":=" {
        return OperatorInfo::new(op, Notation::Infix, Associativity::Right, 6.0);
    }
    // if
    if op == "if" {
        return OperatorInfo::new(op, Notation::None, Associativity::None, 5.0);
    }
    // ;
    if op == ";" {
        return OperatorInfo::new(op, Notation::Infix, Associativity::Right, 4.0);
    }
    // let match fun function try
    if op == "let" || op == "match" || op == "fun" || op == "function" || op == "try" {
        return OperatorInfo::new(op, Notation::None, Associativity::None, 3.0);
    }

    if op == "implies" || op == "==>" {
        return OperatorInfo::new("==>", Notation::Infix, Associativity::Right, 8.3);
    }
    if op == "explies" || op == "<==" {
        return OperatorInfo::new("<==", Notation::Infix, Associativity::Left, 8.2);
    }
    if op == "iff" || op == "<==>" {
        return OperatorInfo::new("<==>", Notation::Infix, Associativity::None, 8.1);
    }

    if op == "List.append" {
        return operator_info("@", more_than_one_arg);
    }

    // function application, constructor application, tag application
    default_oi()
}

pub fn default_oi() -> OperatorInfo {
    OperatorInfo::new("", Notation::Prefix, Associativity::Left, 17.0)
}

type Term<'a> = MirTerm<'a>;

pub fn operator_info_of_term(t: &Term) -> OperatorInfo {
    match t.view {
        MirTermView::If(_, _, _) => operator_info("if", false),
        MirTermView::Apply { f, l } => {
            if let MirTermView::Sym(sym) = f.view {
                operator_info(short_id(sym.sym.id.name), !l.is_empty())
            } else {
                default_oi()
            }
        }
        MirTermView::Tuple { .. } => operator_info(",", false),
        MirTermView::Case { .. } => operator_info("match", false),
        MirTermView::Construct { c, args, .. } => {
            operator_info(short_id(c.sym.id.name), !args.is_empty())
        }
        MirTermView::Const(c) => {
            if let Const::Const_q(q) = c {
                if *q.denom() != num_bigint::BigInt::from(1) {
                    return operator_info("/.", true);
                }
            }
            default_oi()
        }
        MirTermView::Sym(_)
        | MirTermView::Var(_)
        | MirTermView::Destruct { .. }
        | MirTermView::Is_a { .. }
        | MirTermView::Field { .. }
        | MirTermView::Tuple_field { .. }
        | MirTermView::Record { .. }
        | MirTermView::Sequence(_, _) => default_oi(),
    }
}

pub fn needs_parentheses(
    parent_oi: &OperatorInfo,
    child_oi: &OperatorInfo,
    is_left: Option<bool>,
) -> bool {
    (!child_oi.name.is_empty()
        && (parent_oi.precedence > child_oi.precedence
            || (is_left.is_some()
                && child_oi.precedence == parent_oi.precedence
                && ((is_left == Some(true) && parent_oi.associativity == Associativity::Right)
                    || (is_left.is_some()
                        && is_left == Some(false)
                        && parent_oi.associativity == Associativity::Left)))))
        || (parent_oi.notation != Notation::Infix && child_oi.notation == Notation::Infix)
}
