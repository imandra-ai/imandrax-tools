/// Wadler's Pretty Printing Algorithm
/// Based on "A Prettier Printer" by Philip Wadler (1997)
///
/// The algorithm works in two phases:
///   1. Build a Doc tree using the combinators below
///   2. Render to a string with `pretty(width, doc)`

// ---------------------------------------------------------------------------
// Doc — the main algebraic data type
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum Doc {
    Nil,
    Line,
    LineBreak,
    Text(String, usize), // content, visual_length
    Concat(Box<Doc>, Box<Doc>),
    Nest(usize, Box<Doc>),
    Union(Box<Doc>, Box<Doc>), // x flat, y broken
}

// ---------------------------------------------------------------------------
// SimpleDoc — intermediate representation after layout selection
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
enum SimpleDoc {
    SNil,
    SText(String, Box<SimpleDoc>),
    SLine(usize, Box<SimpleDoc>),
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Flatten a document: LINE → space, LINEBREAK → empty, everything else unchanged.
fn flatten(doc: &Doc) -> Doc {
    match doc {
        Doc::Nil => Doc::Nil,
        Doc::Text(s, l) => Doc::Text(s.clone(), *l),
        Doc::Line => Doc::Text(" ".into(), 1),
        Doc::LineBreak => Doc::Nil,
        Doc::Concat(x, y) => concat(flatten(x), flatten(y)),
        Doc::Nest(i, x) => Doc::Nest(*i, Box::new(flatten(x))),
        Doc::Union(x, _) => flatten(x), // x is already the flat branch
    }
}

/// Does `sdoc` fit within `remaining` columns before the next newline?
fn fits(remaining: isize, sdoc: &SimpleDoc) -> bool {
    let mut r = remaining;
    let mut s = sdoc;
    loop {
        if r < 0 {
            return false;
        }
        match s {
            SimpleDoc::SNil => return true,
            SimpleDoc::SLine(_, _) => return true,
            SimpleDoc::SText(text, rest) => {
                r -= text.len() as isize;
                s = rest;
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Core layout algorithm
// ---------------------------------------------------------------------------

type WorkItem = (usize, Doc); // (indent, doc)

/// Select the best layout for a document given a page width `w` and the number
/// of characters `k` already placed on the current line.
fn best(w: usize, k: usize, docs: Vec<WorkItem>) -> SimpleDoc {
    let mut docs = docs;

    loop {
        if docs.is_empty() {
            return SimpleDoc::SNil;
        }

        let (i, doc) = docs.remove(0);

        match doc {
            Doc::Nil => {
                // continue
            }
            Doc::Concat(x, y) => {
                docs.insert(0, (i, *y));
                docs.insert(0, (i, *x));
            }
            Doc::Nest(n, x) => {
                docs.insert(0, (i + n, *x));
            }
            Doc::Text(s, vl) => {
                let rest = best(w, k + vl, docs);
                return SimpleDoc::SText(s, Box::new(rest));
            }
            Doc::Line | Doc::LineBreak => {
                let rest = best(w, i, docs);
                return SimpleDoc::SLine(i, Box::new(rest));
            }
            Doc::Union(x, y) => {
                let flat_docs = {
                    let mut d = vec![(i, *x)];
                    d.extend(docs.clone());
                    d
                };
                let broken_docs = {
                    let mut d = vec![(i, *y)];
                    d.extend(docs);
                    d
                };
                let flat = best(w, k, flat_docs);
                if fits(w as isize - k as isize, &flat) {
                    return flat;
                } else {
                    return best(w, k, broken_docs);
                }
            }
        }
    }
}

/// Render a SimpleDoc to a string.
fn layout(sdoc: SimpleDoc) -> String {
    let mut parts = Vec::new();
    let mut s = sdoc;
    loop {
        match s {
            SimpleDoc::SNil => break,
            SimpleDoc::SText(text, rest) => {
                parts.push(text);
                s = *rest;
            }
            SimpleDoc::SLine(indent, rest) => {
                parts.push("\n".to_string());
                for _ in 0..indent {
                    parts.push("\t".to_string());
                }
                s = *rest;
            }
        }
    }
    parts.join("")
}

// ---------------------------------------------------------------------------
// Public API — primitives
// ---------------------------------------------------------------------------

/// The empty document.
pub fn nil() -> Doc {
    Doc::Nil
}

/// A breakable newline. Inside `group`, becomes a space if the line fits.
pub fn line() -> Doc {
    Doc::Line
}

/// A breakable newline that becomes empty (not a space) when flattened.
pub fn linebreak() -> Doc {
    Doc::LineBreak
}

/// A literal text string (must not contain newlines).
pub fn text(s: &str) -> Doc {
    if s.is_empty() {
        Doc::Nil
    } else {
        let len = s.len();
        Doc::Text(s.to_string(), len)
    }
}

/// A text string with an explicit visual length (for when content length differs from display length).
pub fn vtext(s: &str, visual_length: usize) -> Doc {
    if s.is_empty() {
        Doc::Nil
    } else {
        Doc::Text(s.to_string(), visual_length)
    }
}

// ---------------------------------------------------------------------------
// Public API — combinators
// ---------------------------------------------------------------------------

/// Concatenate two documents.
pub fn concat(x: Doc, y: Doc) -> Doc {
    match (&x, &y) {
        (Doc::Nil, _) => y,
        (_, Doc::Nil) => x,
        _ => Doc::Concat(Box::new(x), Box::new(y)),
    }
}

/// Concatenate an array of documents left-to-right.
pub fn hcat(docs: Vec<Doc>) -> Doc {
    docs.into_iter().fold(Doc::Nil, concat)
}

/// Concatenate documents with `sep` between each pair.
pub fn punctuate(sep: Doc, docs: Vec<Doc>) -> Doc {
    if docs.is_empty() {
        return Doc::Nil;
    }
    let mut iter = docs.into_iter();
    let first = iter.next().unwrap();
    iter.fold(first, |acc, d| concat(acc, concat(sep.clone(), d)))
}

/// Indent nested content by `i` additional tab stops.
pub fn nest(i: usize, x: Doc) -> Doc {
    Doc::Nest(i, Box::new(x))
}

/// Try to lay out the document on a single line; if it doesn't fit within the
/// page width, fall back to the normal (multi-line) layout.
pub fn group(x: Doc) -> Doc {
    let flat = flatten(&x);
    Doc::Union(Box::new(flat), Box::new(x))
}

// ---------------------------------------------------------------------------
// Public API — derived separators
// ---------------------------------------------------------------------------

/// Lay out documents separated by `line` (spaces or newlines).
pub fn vsep(docs: Vec<Doc>) -> Doc {
    punctuate(line(), docs)
}

/// Try to fill a line; break with a newline only when necessary.
pub fn fill(docs: Vec<Doc>) -> Doc {
    punctuate(group(line()), docs)
}

// ---------------------------------------------------------------------------
// Public API — enclosures
// ---------------------------------------------------------------------------

/// Wrap a document between two delimiter documents.
pub fn enclose(l: Doc, r: Doc, x: Doc) -> Doc {
    concat(l, concat(x, r))
}

pub fn parens(x: Doc) -> Doc {
    enclose(text("("), text(")"), x)
}

pub fn brackets(x: Doc) -> Doc {
    enclose(text("["), text("]"), x)
}

pub fn braces(x: Doc) -> Doc {
    enclose(text("{"), text("}"), x)
}

// ---------------------------------------------------------------------------
// Public API — list layouts
// ---------------------------------------------------------------------------

/// Lay out `docs` between `ldelim`/`rdelim` separated by `sep`.
/// Tries the flat layout first; falls back to one item per line.
pub fn enclose_sep(ldelim: Doc, rdelim: Doc, sep: Doc, docs: Vec<Doc>) -> Doc {
    if docs.is_empty() {
        return concat(ldelim, rdelim);
    }
    if docs.len() == 1 {
        let d = docs.into_iter().next().unwrap();
        return concat(ldelim, concat(d, rdelim));
    }
    let mut seps = vec![ldelim];
    for _ in 1..docs.len() {
        seps.push(sep.clone());
    }
    let body = hcat(
        docs.into_iter()
            .enumerate()
            .map(|(i, d)| concat(seps[i].clone(), d))
            .collect(),
    );
    group(concat(body, rdelim))
}

// ---------------------------------------------------------------------------
// Public API — main entry point
// ---------------------------------------------------------------------------

/// Pretty-print `doc` to a string, fitting within `width` columns where possible.
pub fn pretty(width: usize, doc: Doc) -> String {
    layout(best(width, 0, vec![(0, doc)]))
}

// ---------------------------------------------------------------------------
// Helpers (used by formatter)
// ---------------------------------------------------------------------------

pub fn join(e: Doc, docs: Vec<Doc>) -> Doc {
    if docs.is_empty() {
        nil()
    } else if docs.len() == 1 {
        docs.into_iter().next().unwrap()
    } else {
        let mut result = Vec::new();
        let mut first = true;
        for d in docs {
            if !first {
                result.push(e.clone());
            }
            result.push(d);
            first = false;
        }
        hcat(result)
    }
}

pub fn indent(docs: Vec<Doc>) -> Doc {
    nest(1, hcat(docs))
}

pub fn g(docs: Vec<Doc>) -> Doc {
    group(hcat(docs))
}

pub fn par_if(c: bool, docs: Vec<Doc>) -> Doc {
    if c { parens(hcat(docs)) } else { hcat(docs) }
}
