"""
Wadler's Pretty Printing Algorithm.

Based on "A Prettier Printer" by Philip Wadler (1997).

The algorithm works in two phases:
  1. Build a Doc tree using the combinators below.
  2. Render to a string with `pretty(width, doc)`.

Ported from [imandrax-vscode/src/goal-state/term-formatter.ts@7275010ec14d821b2fec0fc2aa0a9fcd84c25c6f]
"""

from __future__ import annotations

from collections.abc import Iterable
from dataclasses import dataclass
from functools import reduce
from typing import Literal, assert_never

PYTHON_INDENT = 2

# Document AST
# =============


@dataclass(slots=True, frozen=True)
class Nil:
    pass


@dataclass(slots=True, frozen=True)
class Text:
    s: str


@dataclass(slots=True, frozen=True)
class Line:
    """Potential line break; flattened to a single space when the group fits."""


@dataclass(slots=True, frozen=True)
class LineBreak:
    """Potential line break; flattened to the empty string when the group fits."""


@dataclass(slots=True, frozen=True)
class HardLine:
    """Unconditional line break; never flattened, and forces enclosing groups to break."""


@dataclass(slots=True, frozen=True)
class Concat:
    left: Doc
    right: Doc


@dataclass(slots=True, frozen=True)
class Nest:
    indent: int
    doc: Doc


@dataclass(slots=True, frozen=True)
class Group:
    """Try to lay out the inner doc flat; fall back to vertical if it doesn't fit."""

    doc: Doc


@dataclass(slots=True, frozen=True)
class FlatAlt:
    default: Doc  # Used when the enclosing group breaks
    flat: Doc  # Used when the enclosing group is flat


@dataclass(slots=True, frozen=True)
class Prefix:
    """
    Like `Nest`, but the indent unit is an arbitrary string, not N spaces.

    Every newline emitted inside `doc` is followed by the accumulated prefix.
    This lets a layout carry non-space gutters (e.g. tree guides `│   `).
    """

    p: str
    doc: Doc


type Doc = (
    Nil | Text | Line | LineBreak | HardLine | Concat | Nest | Group | FlatAlt | Prefix
)

# Smart constructors
# ==================

nil: Doc = Nil()
line: Doc = Line()
"""Potential line break; flattened to a single space when the group fits."""
linebreak: Doc = LineBreak()
"""Potential line break; flattened to the empty string when the group fits."""


def text(s: str) -> Doc:
    return nil if len(s) == 0 else Text(s)


def concat(left: Doc, right: Doc) -> Doc:
    return Concat(left, right)


def nest(i: int, doc: Doc) -> Doc:
    return Nest(i, doc)


def group(doc: Doc) -> Doc:
    return Group(doc)


def flatten(doc: Doc) -> Doc:
    """
    Force `doc` onto a single line, unconditionally.

    Unlike `group` (which lays out flat only when it fits the width), this
    rewrites every potential break to its flat form regardless of width:
    `Line` becomes a space, `LineBreak` vanishes, `Group`/`FlatAlt` collapse to
    their flat branch. `HardLine` cannot be flattened and is left intact.
    """
    match doc:
        case Nil() | Text() | HardLine():
            return doc
        case Line():
            return Text(' ')
        case LineBreak():
            return nil
        case Concat(left, right):
            return Concat(flatten(left), flatten(right))
        case Nest(i, inner):
            return Nest(i, flatten(inner))
        case Group(inner):
            return flatten(inner)
        case FlatAlt(_, flat):
            return flatten(flat)
        case Prefix(p, inner):
            return Prefix(p, flatten(inner))
        case _:
            assert_never(doc)


def flat_alt(default: Doc, flat: Doc) -> Doc:
    return FlatAlt(default, flat)


hardline: Doc = HardLine()
"""A line break that always breaks, regardless of the enclosing group."""


def prefix(p: str, doc: Doc) -> Doc:
    return doc if len(p) == 0 else Prefix(p, doc)


# Iterative `best`
# ----------------
# Wadler's recursive `best` is replaced by an explicit work-stack of
# (indent, mode, doc) triples. Column position is tracked as a mutable counter
# updated whenever a token is emitted.
#
# Modes:
#   FLAT  — newlines become spaces (inside a fitting group)
#   BREAK — newlines are real newlines


FLAT = 0
BREAK = 1
type Mode = Literal[0, 1]

# Token kinds emitted by `best`:
#   ('text', str)      — literal text
#   ('line', str)      — newline followed by the accumulated indent prefix
type SimpleToken = tuple[Literal['text'], str] | tuple[Literal['line'], str]


def _fits(remaining: int, doc: Doc) -> bool:
    stack: list[Doc] = [doc]
    rem = remaining
    while stack:
        if rem < 0:
            return False
        d = stack.pop()
        match d:
            case HardLine():
                # A hard break can never sit on the current line, so any group
                # containing it must lay out broken.
                return False
            case Nil() | LineBreak():
                pass
            case Line():
                rem -= 1
            case Text(s):
                rem -= len(s)
            case Concat(left, right):
                stack.append(right)
                stack.append(left)
            case Nest(_, inner) | Group(inner) | Prefix(_, inner):
                stack.append(inner)
            case FlatAlt(_, flat):
                stack.append(flat)
    return rem >= 0


def _best(width: int, doc: Doc) -> list[SimpleToken]:
    result: list[SimpleToken] = []
    col = 0
    # The accumulator is the indent *prefix* string emitted after each newline,
    # grown by `Nest` (spaces) and `Prefix` (arbitrary text).
    stack: list[tuple[str, Mode, Doc]] = [('', BREAK, doc)]

    while stack:
        pfx, mode, d = stack.pop()
        match d:
            case Nil():
                pass
            case Text(s):
                result.append(('text', s))
                col += len(s)
            case Line():
                if mode == FLAT:
                    result.append(('text', ' '))
                    col += 1
                else:
                    result.append(('line', pfx))
                    col = len(pfx)
            case LineBreak():
                if mode != FLAT:
                    result.append(('line', pfx))
                    col = len(pfx)
            case HardLine():
                # Always a real newline, in either mode, carrying the prefix.
                result.append(('line', pfx))
                col = len(pfx)
            case Concat(left, right):
                stack.append((pfx, mode, right))
                stack.append((pfx, mode, left))
            case Nest(i, inner):
                stack.append((pfx + ' ' * i, mode, inner))
            case FlatAlt(default, flat):
                chosen = flat if mode == FLAT else default
                stack.append((pfx, mode, chosen))
            case Prefix(p, inner):
                stack.append((pfx + p, mode, inner))
            case Group(inner):
                if mode == FLAT:
                    stack.append((pfx, FLAT, inner))
                else:
                    m: Mode = FLAT if _fits(width - col, inner) else BREAK
                    stack.append((pfx, m, inner))
            case _:
                assert_never(d)
    return result


def _layout(tokens: list[SimpleToken]) -> str:
    parts: list[str] = []
    for kind, val in tokens:
        match kind:
            case 'text':
                parts.append(val)
            case 'line':
                parts.append('\n' + val)
    return ''.join(parts)


def pretty(width: int, doc: Doc) -> str:
    return _layout(_best(width, doc))


# Public API -- combinators
# ========================


def hcat(*docs: Doc) -> Doc:
    """Concatenate documents left-to-right."""
    return reduce(concat, docs, nil)


def punctuate(sep: Doc, docs: Iterable[Doc]) -> Doc:
    """Concatenate documents with `sep` between each pair."""
    docs = list(docs)
    if not docs:
        return nil
    it = iter(docs)
    acc = next(it)
    for d in it:
        acc = concat(acc, concat(sep, d))
    return acc


def hsep(docs: Iterable[Doc]) -> Doc:
    """Lay out documents separated by spaces (never breaks)."""
    return punctuate(text(' '), docs)


def vsep(docs: Iterable[Doc]) -> Doc:
    """Lay out documents separated by `line` (spaces or newlines)."""
    return punctuate(line, docs)


def fill(docs: Iterable[Doc]) -> Doc:
    """Try to fill a line; break only when necessary."""
    return punctuate(group(line), docs)


def words_to_doc(ws: list[str]) -> Doc:
    if not ws:
        return nil
    result = text(ws[0])
    for w in ws[1:]:
        result = concat(result, concat(group(line), text(w)))
    return result


# Enclosures
# ===========


def enclose(left: str, right: str, x: Doc) -> Doc:
    return hcat(text(left), x, text(right))


def parens(x: Doc) -> Doc:
    return enclose('(', ')', x)


def brackets(x: Doc) -> Doc:
    return enclose('[', ']', x)


def braces(x: Doc) -> Doc:
    return enclose('{', '}', x)


def angles(x: Doc) -> Doc:
    return enclose('<', '>', x)


def squotes(x: Doc) -> Doc:
    return enclose("'", "'", x)


def dquotes(x: Doc) -> Doc:
    return enclose('"', '"', x)


# List layouts
# ============


def enclose_sep(ldelim: Doc, rdelim: Doc, sep: Doc, docs: list[Doc]) -> Doc:
    """Lay out `docs` between delimiters separated by `sep`. Flat first, else broken."""
    if not docs:
        return concat(ldelim, rdelim)
    if len(docs) == 1:
        return concat(ldelim, concat(docs[0], rdelim))
    seps: list[Doc] = [ldelim] + [sep] * (len(docs) - 1)
    body = hcat(*(concat(seps[i], d) for i, d in enumerate(docs)))
    return group(concat(body, rdelim))


def python_enclose(
    ldelim: Doc,
    rdelim: Doc,
    docs: Iterable[Doc],
    trailing_comma: bool = True,
) -> Doc:
    """
    Python-style layout: one line if it fits, else each item on its own line.

    Flat:   [a, b, c]
    Break:  [
                a,
                b,
                c,
            ]
    """
    docs = list(docs)
    if not docs:
        return concat(ldelim, rdelim)
    sep = concat(text(','), line)
    body = punctuate(sep, docs)
    tcomma = flat_alt(text(','), nil) if trailing_comma else nil
    inner = nest(PYTHON_INDENT, hcat(linebreak, body, tcomma))
    return group(hcat(ldelim, inner, linebreak, rdelim))


def python_dict_entry(k: Doc, v: Doc) -> Doc:
    return hcat(k, text(': '), v)


def python_dict(entries: Iterable[tuple[Doc, Doc]]) -> Doc:
    items = [python_dict_entry(k, v) for k, v in entries]
    return python_enclose(text('{'), text('}'), items)


def python_obj(name: str, fields: Iterable[tuple[str | None, Doc]]) -> Doc:
    """
    Python-style object: `name(arg1=..., arg2=...)`; Breaks into multiple lines with a trailing comma if too wide.

    Args:
        name: the object name
        fields: associated list of `(name, value)` pairs. If `name` is `None`, the value is treated like a positional arg.

    """
    parts = [concat(text(f'{k}='), v) if k is not None else v for k, v in fields]
    return python_enclose(text(f'{name}('), text(')'), parts)


def list_doc(docs: Iterable[Doc]) -> Doc:
    """Python-style list: `[a, b, c]` flat or one-per-line broken."""
    return python_enclose(text('['), text(']'), docs)


def tupled(docs: Iterable[Doc]) -> Doc:
    """Python-style tuple: `(a, b, c)` flat or one-per-line broken."""
    return python_enclose(text('('), text(')'), list(docs))


def assoc_list(docs: Iterable[tuple[str, Doc]]) -> Doc:
    """Python-style dict: `{k: v, ...}` flat or one-per-line broken."""
    items: list[Doc] = [concat(concat(text(k), text(': ')), v) for (k, v) in docs]
    return python_enclose(text('{'), text('}'), items)


# Tree layout
# ===========

# mid, end, gutter_mid, gutter_end
TREE_CHARS = (
    '├─ ',
    '└─ ',
    '│  ',
    '   ',
)

TREE_CHARS_ASCII = (
    '|-- ',
    '`-- ',
    '|   ',
    '    ',
)


def tree(header: Doc, children: list[Doc], ascii_only: bool = False) -> Doc:
    """
    Lay out `header` with `children` hung beneath it using box-drawing guides.

    Each child's first line is introduced by a connector (`├─`/`└─`); its
    continuation lines carry the matching gutter (`│ `/spaces) so nested
    subtrees stay aligned under their connector. Uses `hardline`, so the tree
    always breaks regardless of the enclosing group.
    """
    mid, end, gutter_mid, gutter_end = TREE_CHARS_ASCII if ascii_only else TREE_CHARS
    parts: list[Doc] = [header]
    last = len(children) - 1
    for i, child in enumerate(children):
        is_last = i == last
        conn = end if is_last else mid
        gutter = gutter_end if is_last else gutter_mid
        parts.append(hcat(hardline, text(conn), prefix(gutter, child)))
    return hcat(*parts)


def python_quote(
    inner: Doc,
    indent: int = PYTHON_INDENT,
    single_quote: bool = True,
) -> Doc:
    q = flat_alt(text("'''"), text("'") if single_quote else text('"'))
    body = nest(indent, concat(linebreak, inner))
    return group(hcat(q, body, linebreak, q))


# Helpers
# -------


def join(e: Doc, docs: list[Doc]) -> Doc:
    if not docs:
        return nil
    if len(docs) == 1:
        return docs[0]
    r: list[Doc] = [docs[0]]
    for d in docs[1:]:
        r.append(e)
        r.append(d)
    return hcat(*r)
