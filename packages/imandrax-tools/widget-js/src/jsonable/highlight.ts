// Syntax highlighting for one YAML line, hand-rolled — same trade as
// `task/highlight.ts`: the input grammar is narrow (block-style YAML emitted by
// PyYAML via `yaml_utils.to_yaml_str`, never hand-written), so a few regexes beat
// pulling a highlighting library into the bundle.
//
// Line-wise rather than document-wise, which is what makes the fold structure in
// `fold.ts` usable: every visible line is tokenized independently, and block
// scalar bodies are handed to `highlightBlockLine` instead, which only escapes.
//
// `highlightLine` returns an HTML string of `<span class="t-...">` tokens; every
// character of the input survives, escaped, so `textContent` of the result is
// byte-for-byte the original line.

// Leading `- ` sequence markers (`- - a` for nested sequences).
const DASHES = /^(?:-(?:[ \t]+|$))+/;
// `key:` — quoted or plain. Plain keys stop at the first `:`, which is safe
// because PyYAML quotes any key containing one.
const KEY = /^("(?:[^"\\]|\\.)*"|'(?:[^']|'')*'|[^:#\s][^:]*?)(:)([ \t]|$)/;
// A block-scalar indicator standing alone as the value.
const BLOCK_IND = /^[|>][+-]?\d{0,2}$/;
// Anchors, aliases, and tags, which may prefix a value (`&a`, `*a`, `!!str`).
const REF = /^([&*]\S+|!!?\S*)([ \t]+|$)/;
const NUM = /^-?(?:\d[\d_]*(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$|^-?0[xXoObB][0-9a-fA-F_]+$|^[-+]?\.(?:inf|Inf|INF)$|^\.(?:nan|NaN|NAN)$/;
const LIT = /^(?:true|True|TRUE|false|False|FALSE|null|Null|NULL|~)$/;
// A quoted scalar at the head of a value, used to skip over `#` inside quotes.
const LEADING_QUOTED = /^"(?:[^"\\]|\\.)*"|^'(?:[^']|'')*'/;

export function escapeHtml(s: string): string {
  return s.replace(/[&<>]/g, (c) => (c === '&' ? '&amp;' : c === '<' ? '&lt;' : '&gt;'));
}

function tok(cls: string, text: string): string {
  return `<span class="t-${cls}">${escapeHtml(text)}</span>`;
}

/**
 * Split a value off its trailing comment.
 *
 * A `#` only starts a comment when preceded by whitespace and outside quotes —
 * `msg: 'a # b'` is one string, `x: 1 # note` is a value plus a comment.
 */
function splitComment(v: string): [string, string] {
  const lead = /^[ \t]*/.exec(v)![0].length;
  // Skip a leading quoted scalar; a plain scalar cannot contain ` #` at all.
  const quoted = LEADING_QUOTED.exec(v.slice(lead));
  const from = lead + (quoted ? quoted[0].length : 0);
  const at = /(?:^|[ \t])#/.exec(v.slice(from));
  if (!at) return [v, ''];
  const cut = from + at.index; // any whitespace before `#` goes with the comment
  return [v.slice(0, cut), v.slice(cut)];
}

/** Highlight a scalar value (everything after `key:` or a `-`). */
function value(v: string): string {
  const [raw, comment] = splitComment(v);
  const lead = /^[ \t]*/.exec(raw)![0];
  let body = raw.slice(lead.length);
  let out = lead;

  // An anchor/alias/tag can precede the scalar; emit it and continue.
  const ref = REF.exec(body);
  if (ref) {
    out += tok('ref', ref[1]) + ref[2];
    body = body.slice(ref[0].length);
  }

  if (body) {
    const cls = BLOCK_IND.test(body)
      ? 'block'
      : LIT.test(body)
        ? 'lit'
        : NUM.test(body)
          ? 'num'
          : 'str'; // quoted and plain scalars alike
    out += tok(cls, body);
  }
  return out + (comment ? tok('comment', comment) : '');
}

export function highlightLine(line: string): string {
  const indent = /^[ \t]*/.exec(line)![0];
  let rest = line.slice(indent.length);
  let out = indent;

  if (!rest) return out;

  // Document markers stand alone.
  if (rest === '---' || rest === '...') return out + tok('punct', rest);

  const dashes = DASHES.exec(rest);
  if (dashes) {
    out += tok('punct', dashes[0]);
    rest = rest.slice(dashes[0].length);
  }

  if (rest.startsWith('#')) return out + tok('comment', rest);

  const key = KEY.exec(rest);
  if (key) {
    out += tok('key', key[1]) + tok('punct', ':');
    return out + value(rest.slice(key[1].length + 1));
  }

  // No key: a sequence item's scalar, or a plain-scalar continuation line.
  return out + value(rest);
}

/** Block-scalar body lines are opaque text — escape only, never tokenize. */
export function highlightBlockLine(line: string): string {
  return escapeHtml(line);
}
