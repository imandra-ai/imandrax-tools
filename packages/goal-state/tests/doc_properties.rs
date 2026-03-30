use goal_state::doc::*;
use hegel::TestCase;
use hegel::generators as gs;

// Doc generators
// ====================

/// Generate a short, newline-free string suitable for `text()`.
fn gen_atom(tc: &TestCase) -> Doc {
    let s = tc.draw(gs::text().min_size(1).max_size(8));
    // strip newlines/tabs so text() invariant holds
    let s: String = s.chars().filter(|c| *c != '\n' && *c != '\t').collect();
    if s.is_empty() { text("a") } else { text(&s) }
}

fn gen_width(tc: &TestCase) -> usize {
    tc.draw(gs::integers::<usize>().min_value(1).max_value(120))
}

// Algebraic laws
// ====================

#[hegel::test]
fn nil_is_identity_for_concat_left(tc: TestCase) {
    let w = gen_width(&tc);
    let x = gen_atom(&tc);
    assert_eq!(pretty(w, concat(nil(), x.clone())), pretty(w, x));
}

#[hegel::test]
fn nil_is_identity_for_concat_right(tc: TestCase) {
    let w = gen_width(&tc);
    let x = gen_atom(&tc);
    assert_eq!(pretty(w, concat(x.clone(), nil())), pretty(w, x));
}

#[hegel::test]
fn hcat_associativity(tc: TestCase) {
    let w = gen_width(&tc);
    let a = gen_atom(&tc);
    let b = gen_atom(&tc);
    let c = gen_atom(&tc);
    let via_hcat = pretty(w, hcat(vec![a.clone(), b.clone(), c.clone()]));
    let via_nested = pretty(w, concat(a, concat(b, c)));
    assert_eq!(via_hcat, via_nested);
}

#[hegel::test]
fn nil_renders_empty(tc: TestCase) {
    let w = gen_width(&tc);
    assert_eq!(pretty(w, nil()), "");
}

#[hegel::test]
fn text_round_trips(tc: TestCase) {
    let w = gen_width(&tc);
    let s = tc.draw(gs::text().min_size(0).max_size(20));
    let s: String = s.chars().filter(|c| *c != '\n' && *c != '\t').collect();
    if !s.is_empty() {
        assert_eq!(pretty(w, text(&s)), s);
    } else {
        // text("") => Nil => ""
        assert_eq!(pretty(w, text(&s)), "");
    }
}

// Core group/layout contract
// ====================

#[hegel::test]
fn group_fits_single_line(tc: TestCase) {
    // Build a small doc that should fit in a wide width
    let a = gen_atom(&tc);
    let b = gen_atom(&tc);
    let doc = group(hcat(vec![a, line(), b]));
    let result = pretty(200, doc);
    assert!(
        !result.contains('\n'),
        "Expected single line at width 200, got: {:?}",
        result
    );
}

#[hegel::test]
fn group_broken_respects_width(tc: TestCase) {
    // Build a doc with several atoms separated by line breaks
    let atoms: Vec<Doc> = (0..5).map(|_| gen_atom(&tc)).collect();
    let body = punctuate(line(), atoms);
    let doc = group(body);
    let w = tc.draw(gs::integers::<usize>().min_value(1).max_value(40));
    let result = pretty(w, doc);
    for (i, l) in result.lines().enumerate() {
        // first line may start mid-way; subsequent lines should respect width
        // (tab = 1 column for measuring purposes here)
        if i > 0 {
            let visual_len: usize = l.chars().count();
            // generous bound: width + one atom overflow is acceptable
            assert!(
                visual_len <= w + 20,
                "Line {} too long ({} > {}+20): {:?}",
                i,
                visual_len,
                w,
                l
            );
        }
    }
}

#[hegel::test]
fn monotonicity_wider_no_more_breaks(tc: TestCase) {
    let atoms: Vec<Doc> = (0..4).map(|_| gen_atom(&tc)).collect();
    let doc = group(punctuate(line(), atoms));
    let narrow_w = tc.draw(gs::integers::<usize>().min_value(1).max_value(40));
    let wide_w = tc.draw(gs::integers::<usize>().min_value(narrow_w).max_value(200));
    let narrow_breaks = pretty(narrow_w, doc.clone()).matches('\n').count();
    let wide_breaks = pretty(wide_w, doc).matches('\n').count();
    assert!(
        wide_breaks <= narrow_breaks,
        "Wider width ({}) produced more breaks ({}) than narrow ({}, {})",
        wide_w,
        wide_breaks,
        narrow_w,
        narrow_breaks
    );
}

// line vs linebreak distinction
// ====================

#[hegel::test]
fn line_becomes_space_in_flat_group(tc: TestCase) {
    let a = gen_atom(&tc);
    let b = gen_atom(&tc);
    let doc = group(hcat(vec![a.clone(), line(), b.clone()]));
    let result = pretty(200, doc);
    let expected = format!("{} {}", pretty(200, a), pretty(200, b));
    assert_eq!(result, expected);
}

#[hegel::test]
fn linebreak_becomes_nothing_in_flat_group(tc: TestCase) {
    let a = gen_atom(&tc);
    let b = gen_atom(&tc);
    let doc = group(hcat(vec![a.clone(), linebreak(), b.clone()]));
    let result = pretty(200, doc);
    let expected = format!("{}{}", pretty(200, a), pretty(200, b));
    assert_eq!(result, expected);
}

#[hegel::test]
fn line_and_linebreak_both_break_when_forced(tc: TestCase) {
    let a = gen_atom(&tc);
    let b = gen_atom(&tc);
    let doc_line = group(hcat(vec![a.clone(), line(), b.clone()]));
    let doc_lb = group(hcat(vec![a, linebreak(), b]));
    // Use width 1 to force a break
    let r_line = pretty(1, doc_line);
    let r_lb = pretty(1, doc_lb);
    assert!(r_line.contains('\n'), "line should break at width 1");
    assert!(r_lb.contains('\n'), "linebreak should break at width 1");
}

// nest indentation
// ====================

#[hegel::test]
fn nest_adds_tabs(tc: TestCase) {
    let n = tc.draw(gs::integers::<usize>().min_value(1).max_value(4));
    let doc = nest(n, hcat(vec![text("a"), line(), text("b")]));
    let result = pretty(1, doc); // force break
    let lines: Vec<&str> = result.lines().collect();
    assert!(lines.len() >= 2, "Expected at least 2 lines");
    let indent = lines[1].chars().take_while(|c| *c == '\t').count();
    assert_eq!(
        indent, n,
        "Expected {} tabs on second line, got {}",
        n, indent
    );
}

#[hegel::test]
fn nest_composes_additively(tc: TestCase) {
    let n1 = tc.draw(gs::integers::<usize>().min_value(1).max_value(3));
    let n2 = tc.draw(gs::integers::<usize>().min_value(1).max_value(3));
    let doc = nest(
        n1,
        hcat(vec![
            text("a"),
            line(),
            nest(n2, hcat(vec![text("b"), line(), text("c")])),
        ]),
    );
    let result = pretty(1, doc);
    let lines: Vec<&str> = result.lines().collect();
    assert!(lines.len() >= 3, "Expected at least 3 lines");
    // line after "b" should have n1 tabs
    let indent_b = lines[1].chars().take_while(|c| *c == '\t').count();
    assert_eq!(indent_b, n1);
    // line after "c" should have n1+n2 tabs
    let indent_c = lines[2].chars().take_while(|c| *c == '\t').count();
    assert_eq!(indent_c, n1 + n2);
}

// Combinators
// ====================

#[hegel::test]
fn enclose_sep_empty_delimiters(tc: TestCase) {
    let w = gen_width(&tc);
    let result = pretty(w, enclose_sep(text("["), text("]"), text(", "), vec![]));
    assert_eq!(result, "[]");
}

#[hegel::test]
fn enclose_sep_single_element(tc: TestCase) {
    let w = gen_width(&tc);
    let x = gen_atom(&tc);
    let x_str = pretty(200, x.clone());
    let result = pretty(w, enclose_sep(text("["), text("]"), text(", "), vec![x]));
    assert_eq!(result, format!("[{}]", x_str));
}

#[hegel::test]
fn enclose_sep_many_contains_all_elements(tc: TestCase) {
    let n = tc.draw(gs::integers::<usize>().min_value(2).max_value(6));
    let atoms: Vec<Doc> = (0..n).map(|_| gen_atom(&tc)).collect();
    let atom_strs: Vec<String> = atoms.iter().map(|a| pretty(200, a.clone())).collect();
    let w = gen_width(&tc);
    let result = pretty(w, enclose_sep(text("["), text("]"), text(", "), atoms));
    // Every atom must appear in the output
    for s in &atom_strs {
        assert!(
            result.contains(s.as_str()),
            "Output {:?} missing atom {:?}",
            result,
            s
        );
    }
    // Must start with [ and end with ]
    assert!(result.starts_with('['), "Should start with [");
    assert!(result.ends_with(']'), "Should end with ]");
}

#[hegel::test]
fn punctuate_interleaves_separator(tc: TestCase) {
    let atoms: Vec<Doc> = (0..3).map(|_| gen_atom(&tc)).collect();
    let atom_strs: Vec<String> = atoms.iter().map(|a| pretty(200, a.clone())).collect();
    let doc = punctuate(text("|"), atoms);
    let result = pretty(200, doc);
    let expected = atom_strs.join("|");
    assert_eq!(result, expected);
}

#[hegel::test]
fn parens_wraps(tc: TestCase) {
    let x = gen_atom(&tc);
    let x_str = pretty(200, x.clone());
    assert_eq!(pretty(200, parens(x)), format!("({})", x_str));
}

#[hegel::test]
fn brackets_wraps(tc: TestCase) {
    let x = gen_atom(&tc);
    let x_str = pretty(200, x.clone());
    assert_eq!(pretty(200, brackets(x)), format!("[{}]", x_str));
}

#[hegel::test]
fn braces_wraps(tc: TestCase) {
    let x = gen_atom(&tc);
    let x_str = pretty(200, x.clone());
    assert_eq!(pretty(200, braces(x)), format!("{{{}}}", x_str));
}
