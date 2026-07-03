// Syntax highlighting for artifact text. Artifacts are always Python `repr()`
// output of the API's dataclasses/pydantic models (see the Python side), i.e.
// `ClassName(kwarg=value, ...)` trees. That's a narrow enough grammar to tokenize
// by hand in a single regex pass — no highlighting library, ~nothing added to the
// bundle. `highlightRepr` returns an HTML string of `<span class="t-...">` tokens.

// One alternation, tried left-to-right at each position. Order matters:
//   - strings first, so quotes/`=`/`(` inside them aren't mistaken for structure;
//     triple-quoted blocks (multi-line proof text) before single-quoted;
//   - `None`/`True`/`False` before the identifier rules (they'd match `ident`);
//   - `cls` (identifier before `(`) and `attr` (identifier before `=`) before the
//     bare-`ident` catch-all, which consumes any remaining identifier *whole* so a
//     digit inside a name (e.g. `_x_0`) is never picked up by `num`.
const TOKEN = new RegExp(
  [
    /(?<str>'''[\s\S]*?'''|"""[\s\S]*?"""|'(?:[^'\\]|\\.)*'|"(?:[^"\\]|\\.)*")/,
    /(?<lit>\b(?:None|True|False)\b)/,
    /(?<cls>[A-Za-z_]\w*(?=\())/,
    /(?<attr>[A-Za-z_]\w*(?=\s*=))/,
    /(?<ident>[A-Za-z_]\w*)/,
    /(?<num>-?\d+(?:\.\d+)?)/,
  ]
    .map((r) => r.source)
    .join('|'),
  'g',
);

const CLASS: Record<string, string> = {
  str: 't-str',
  lit: 't-lit',
  cls: 't-cls',
  attr: 't-attr',
  num: 't-num',
};

function escapeHtml(s: string): string {
  return s.replace(/[&<>]/g, (c) => (c === '&' ? '&amp;' : c === '<' ? '&lt;' : '&gt;'));
}

export function highlightRepr(text: string): string {
  let out = '';
  let last = 0;
  for (let m = TOKEN.exec(text); m; m = TOKEN.exec(text)) {
    out += escapeHtml(text.slice(last, m.index)); // plain text between tokens
    const groups = m.groups ?? {};
    const kind = Object.keys(CLASS).find((k) => groups[k] !== undefined);
    // `ident` (bare identifier) and any other match fall through as plain text.
    out += kind ? `<span class="${CLASS[kind]}">${escapeHtml(m[0])}</span>` : escapeHtml(m[0]);
    last = m.index + m[0].length;
  }
  out += escapeHtml(text.slice(last));
  return out;
}
