use goal_state::doc::*;

// Helper: build a function-call-like doc: `f(arg1, arg2, ...)`
fn fun_call(name: &str, args: Vec<&str>) -> Doc {
    let arg_docs: Vec<Doc> = args.into_iter().map(text).collect();
    g(vec![
        text(name),
        text("("),
        indent(vec![join(hcat(vec![text(","), line()]), arg_docs)]),
        text(")"),
    ])
}

// Helper: if-then-else doc
fn if_then_else(cond: Doc, then: Doc, els: Doc) -> Doc {
    g(vec![
        g(vec![
            text("if"),
            indent(vec![line(), cond]),
            line(),
            text("then"),
        ]),
        indent(vec![line(), then]),
        line(),
        text("else"),
        indent(vec![line(), els]),
    ])
}

#[test]
fn nil_is_empty() {
    insta::assert_snapshot!(pretty(80, nil()), @"");
}

#[test]
fn text_roundtrips() {
    insta::assert_snapshot!(pretty(80, text("hello")), @"hello");
}

#[test]
fn concat_with_nil_identity() {
    let d = text("x");
    // concat(nil, x) == x
    insta::assert_snapshot!(pretty(80, concat(nil(), text("x"))), @"x");
    // concat(x, nil) == x
    insta::assert_snapshot!(pretty(80, concat(d, nil())), @"x");
}

#[test]
fn fun_call_flat() {
    let doc = fun_call("f", vec!["a", "b", "c"]);
    insta::assert_snapshot!(pretty(80, doc), @"f(a, b, c)");
}

#[test]
fn fun_call_broken() {
    let doc = fun_call("f", vec!["a", "b", "c"]);
    insta::assert_snapshot!(pretty(5, doc), @r"
    f(a,
    	b,
    	c)
    ");
}

#[test]
fn fun_call_long_args_flat() {
    let doc = fun_call("compute", vec!["alpha", "beta", "gamma"]);
    insta::assert_snapshot!(pretty(80, doc), @"compute(alpha, beta, gamma)");
}

#[test]
fn fun_call_long_args_broken() {
    let doc = fun_call("compute", vec!["alpha", "beta", "gamma"]);
    insta::assert_snapshot!(pretty(20, doc), @r"
    compute(alpha,
    	beta,
    	gamma)
    ");
}

#[test]
fn nested_fun_call() {
    let inner = fun_call("g", vec!["x", "y"]);
    let outer = g(vec![text("f"), text("("), indent(vec![inner]), text(")")]);
    insta::assert_snapshot!(pretty(80, outer.clone()), @"f(g(x, y))");
    insta::assert_snapshot!(pretty(10, outer), @"f(g(x, y))");
}

#[test]
fn if_then_else_flat() {
    let doc = if_then_else(text("x > 0"), text("x"), text("-x"));
    insta::assert_snapshot!(pretty(80, doc), @"if x > 0 then x else -x");
}

#[test]
fn if_then_else_broken() {
    let doc = if_then_else(text("x > 0"), text("x"), text("-x"));
    insta::assert_snapshot!(pretty(15, doc), @r"
    if x > 0 then
    	x
    else
    	-x
    ");
}

#[test]
fn enclose_sep_empty() {
    let doc = enclose_sep(text("["), text("]"), text(", "), vec![]);
    insta::assert_snapshot!(pretty(80, doc), @"[]");
}

#[test]
fn enclose_sep_single() {
    let doc = enclose_sep(text("["), text("]"), text(", "), vec![text("x")]);
    insta::assert_snapshot!(pretty(80, doc), @"[x]");
}

#[test]
fn enclose_sep_many_flat() {
    let doc = enclose_sep(
        text("["),
        text("]"),
        text(", "),
        vec![text("1"), text("2"), text("3")],
    );
    insta::assert_snapshot!(pretty(80, doc), @"[1, 2, 3]");
}

#[test]
fn enclose_sep_many_broken() {
    let doc = enclose_sep(
        text("["),
        text("]"),
        text(", "),
        vec![text("1"), text("2"), text("3")],
    );
    insta::assert_snapshot!(pretty(5, doc), @"[1, 2, 3]");
}

#[test]
fn line_vs_linebreak_flat() {
    // group fits: line→space, linebreak→nothing
    let with_line = group(hcat(vec![text("a"), line(), text("b")]));
    let with_linebreak = group(hcat(vec![text("a"), linebreak(), text("b")]));
    insta::assert_snapshot!(pretty(80, with_line), @"a b");
    insta::assert_snapshot!(pretty(80, with_linebreak), @"ab");
}

#[test]
fn line_vs_linebreak_broken() {
    // group doesn't fit: both break
    let with_line = group(hcat(vec![text("aaa"), line(), text("bbb")]));
    let with_linebreak = group(hcat(vec![text("aaa"), linebreak(), text("bbb")]));
    insta::assert_snapshot!(pretty(3, with_line), @r"
    aaa
    bbb
    ");
    insta::assert_snapshot!(pretty(3, with_linebreak), @r"
    aaa
    bbb
    ");
}

#[test]
fn nest_indentation() {
    // force a break inside nest(2, ...)
    let doc = nest(2, hcat(vec![text("a"), line(), text("b")]));
    insta::assert_snapshot!(pretty(1, doc), @r"
    a
    		b
    ");
}

#[test]
fn nest_composes() {
    let doc = nest(
        1,
        hcat(vec![
            text("a"),
            line(),
            nest(1, hcat(vec![text("b"), line(), text("c")])),
        ]),
    );
    insta::assert_snapshot!(pretty(1, doc), @r"
    a
    	b
    		c
    ");
}

#[test]
fn parens_brackets_braces() {
    insta::assert_snapshot!(pretty(80, parens(text("x"))), @"(x)");
    insta::assert_snapshot!(pretty(80, brackets(text("x"))), @"[x]");
    insta::assert_snapshot!(pretty(80, braces(text("x"))), @"{x}");
}

#[test]
fn infix_expression() {
    // a + b * c  style, built manually
    let mul = g(vec![text("b"), line(), text("*"), line(), text("c")]);
    let add = g(vec![text("a"), line(), text("+"), line(), parens(mul)]);
    insta::assert_snapshot!(pretty(80, add.clone()), @"a + (b * c)");
    insta::assert_snapshot!(pretty(8, add), @r"
    a
    +
    (b * c)
    ");
}

#[test]
fn deeply_nested_breaks() {
    let doc = fun_call("f", vec!["aaaa", "bbbb", "cccc", "dddd", "eeee"]);
    insta::assert_snapshot!(pretty(15, doc), @r"
    f(aaaa,
    	bbbb,
    	cccc,
    	dddd,
    	eeee)
    ");
}
