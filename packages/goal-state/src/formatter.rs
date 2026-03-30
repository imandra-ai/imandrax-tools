//! Ported from `imandrax-vscode/src/goal-state/term-formatter.ts` part 2
use imandrax_api::*;
use num_bigint::BigInt;

use crate::doc::*;
use crate::operators::*;

type Term<'a> = MirTerm<'a>;
type AppliedSymbol<'a> = CommonApplied_symbolT_poly<'a, &'a MirType<'a>>;

fn kw(w: &str) -> Doc {
    text(w)
}

pub struct TermFormatter {
    width: usize,
}

impl TermFormatter {
    pub fn new(width: usize) -> Self {
        Self { width }
    }

    fn sym2doc(&self, s: &AppliedSymbol) -> Doc {
        let sid = short_id(s.sym.id.name);
        let oi = operator_info(sid, false);
        let op_name = if oi.name.is_empty() { sid } else { &oi.name };
        text(op_name)
    }

    fn const2doc(&self, c: &Const) -> Doc {
        match c {
            Const::Const_float(v) => text(&v.to_string()),
            Const::Const_string(v) => text(&format!("\"{}\"", v)),
            Const::Const_z(v) => text(&v.to_string()),
            Const::Const_q(q) => {
                let num = q.numer().to_string();
                let den = q.denom();
                if *den == BigInt::from(1) {
                    text(&format!("{}.0", num))
                } else {
                    text(&format!("{}.0 /. {}.0", num, den))
                }
            }
            Const::Const_real_approx(v) => text(v),
            Const::Const_uid(v) => text(v.name),
            Const::Const_bool(v) => text(if *v { "true" } else { "false" }),
        }
    }

    pub fn term2doc(&self, t: &Term) -> Doc {
        let rec = |x: &Term| self.term2doc(x);
        let recwp = |parent_oi: &OperatorInfo, x: &Term, is_left: Option<bool>| {
            let child_oi = operator_info_of_term(x);
            let needs_par = needs_parentheses(parent_oi, &child_oi, is_left)
                || (matches!(
                    x.view,
                    MirTermView::Const(Const::Const_q(q))
                        if *q.denom() != BigInt::from(1)
                ) && parent_oi.precedence > operator_info("/.", false).precedence);
            par_if(needs_par, vec![self.term2doc(x)])
        };

        match t.view {
            MirTermView::Const(c) => self.const2doc(c),

            MirTermView::If(cond, then, els) => g(vec![
                g(vec![
                    kw("if"),
                    indent(vec![line(), rec(cond)]),
                    line(),
                    kw("then"),
                ]),
                indent(vec![line(), rec(then)]),
                line(),
                kw("else"),
                indent(vec![line(), rec(els)]),
            ]),

            MirTermView::Apply { f, l } => {
                let fn_doc = rec(f);
                if let MirTermView::Sym(sym) = f.view {
                    let sid = short_id(sym.sym.id.name);
                    let pi = operator_info(sid, l.len() > 1);

                    if l.is_empty() {
                        fn_doc
                    } else {
                        match pi.notation {
                            Notation::Infix => {
                                if l.len() == 2 {
                                    let lhs = recwp(&pi, l[0], Some(true));
                                    let rhs = recwp(&pi, l[1], Some(false));
                                    g(vec![lhs, line(), fn_doc, line(), rhs])
                                } else {
                                    let hargs = indent(vec![
                                        line(),
                                        join(
                                            line(),
                                            l.iter().map(|x| recwp(&pi, x, None)).collect(),
                                        ),
                                    ]);
                                    g(vec![text("( "), fn_doc, text(" )"), hargs])
                                }
                            }
                            _ => {
                                let hargs = indent(vec![
                                    line(),
                                    join(line(), l.iter().map(|x| recwp(&pi, x, None)).collect()),
                                ]);
                                if l.is_empty() {
                                    fn_doc
                                } else {
                                    g(vec![fn_doc, hargs])
                                }
                            }
                        }
                    }
                } else {
                    // fn is a function term
                    let hargs = indent(vec![
                        line(),
                        join(
                            line(),
                            l.iter().map(|x| recwp(&default_oi(), x, None)).collect(),
                        ),
                    ]);
                    par_if(!l.is_empty(), vec![fn_doc, hargs])
                }
            }

            MirTermView::Var(var) => {
                let sid = short_id(var.id.name);
                text(sid)
            }

            MirTermView::Sym(sym) => self.sym2doc(sym),

            MirTermView::Construct { c, args, .. } => {
                if args.is_empty() {
                    text(short_id(c.sym.id.name))
                } else {
                    let op_doc = self.sym2doc(c);
                    let sid = short_id(c.sym.id.name);
                    let pi = operator_info(sid, args.len() > 1);
                    match pi.notation {
                        Notation::Infix => {
                            if args.len() == 2 {
                                let lhs = recwp(&pi, args[0], Some(true));
                                let rhs = recwp(&pi, args[1], Some(false));
                                g(vec![lhs, line(), op_doc, line(), rhs])
                            } else {
                                let hargs = indent(vec![
                                    line(),
                                    join(
                                        line(),
                                        args.iter().map(|x| recwp(&pi, x, None)).collect(),
                                    ),
                                ]);
                                g(vec![text("( "), op_doc, text(" )"), hargs])
                            }
                        }
                        _ => {
                            let arg_docs: Vec<Doc> = args.iter().map(|a| rec(a)).collect();
                            g(vec![par_if(
                                true,
                                vec![op_doc, indent(vec![line(), join(line(), arg_docs)])],
                            )])
                        }
                    }
                }
            }

            MirTermView::Destruct { c, i, t: term } => parens(g(vec![
                kw("destruct"),
                brackets(hcat(vec![self.sym2doc(c), text("|"), text(&i.to_string())])),
                line(),
                rec(term),
            ])),

            MirTermView::Is_a { c, t: term } => g(vec![
                rec(term),
                line(),
                kw("is_a"),
                indent(vec![line(), self.sym2doc(c)]),
            ]),

            MirTermView::Tuple { l } => g(vec![parens(indent(vec![join(
                hcat(vec![text(","), line()]),
                l.iter().map(|x| rec(x)).collect(),
            )]))]),

            MirTermView::Field { f, t: term } => g(vec![
                rec(term),
                text("."),
                linebreak(),
                text(short_id(f.sym.id.name)),
            ]),

            MirTermView::Tuple_field { i, t: term } => g(vec![
                rec(term),
                text("."),
                linebreak(),
                text(&i.to_string()),
            ]),

            MirTermView::Record { rows, rest } => {
                let hrows = join(
                    hcat(vec![text(";"), line()]),
                    rows.iter()
                        .map(|(sym, term)| g(vec![self.sym2doc(sym), text(":"), rec(term)]))
                        .collect(),
                );
                let hrest = match rest {
                    Some(r) => g(vec![rec(r), line(), kw("with"), line()]),
                    None => nil(),
                };
                braces(hcat(vec![hrest, hrows]))
            }

            MirTermView::Case { u, cases, default } => {
                let hcases = join(
                    line(),
                    cases
                        .iter()
                        .map(|(sym, term)| {
                            g(vec![
                                text("|"),
                                line(),
                                self.sym2doc(sym),
                                line(),
                                text("->"),
                                line(),
                                rec(term),
                            ])
                        })
                        .collect(),
                );
                let hdefault = match default {
                    Some(d) => g(vec![
                        line(),
                        text("|"),
                        line(),
                        text("_"),
                        line(),
                        text("->"),
                        line(),
                        rec(d),
                    ]),
                    None => nil(),
                };
                g(vec![
                    kw("case"),
                    line(),
                    rec(u),
                    kw("of"),
                    line(),
                    hcases,
                    hdefault,
                ])
            }

            MirTermView::Sequence(stmts, last) => {
                let hseq = join(
                    hcat(vec![text(";"), line()]),
                    stmts.iter().map(|x| rec(x)).collect(),
                );
                g(vec![hseq, rec(last)])
            }
        }
    }

    pub fn prettify(&self, t: &Term) -> String {
        pretty(self.width, self.term2doc(t))
    }
}

/// Pretty-print term `t` with `width` line size.
pub fn prettify(width: usize, t: &Term) -> String {
    TermFormatter::new(width).prettify(t)
}
