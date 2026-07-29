// Fold structure for a YAML document, derived from indentation alone.
//
// The Python side is the only YAML formatter (`yaml_utils.to_yaml_str`), so the
// widget receives *text*, not a tree. Rather than parse YAML, we recover the
// nesting the way an editor's code folding does: a line owns every following
// line indented past it. That is exactly the structure a reader wants to
// collapse, and it stays correct for constructs a real parser would be needed
// for (anchors, tags, flow collections) because they never break the indent
// invariant.
//
// Block scalars (`key: |`) are the one construct handled specially: their body
// is arbitrary text (ImandraX proof output, source snippets), so it is captured
// verbatim and never highlighted or folded internally.

export interface YamlNode {
  /** The source line, verbatim — leading indentation included. */
  text: string;
  /** Number of leading spaces. */
  indent: number;
  /** Lines nested under this one. */
  children: YamlNode[];
  /**
   * Body of a block scalar (`|`, `>`) opened by this line, verbatim. Empty for
   * every other line. Rendered as raw text, never tokenized.
   */
  block: string[];
}

const BLANK = /^\s*$/;
// One or more `- ` sequence markers at the head of a line. A sequence item's
// siblings hang off the *content* column (after the dash), not the dash column.
const DASHES = /^(?:-(?:\s+|$))+/;
// A mapping key with no inline value (`region_groups:`), i.e. one whose value is
// the block that follows. PyYAML writes such a key's sequence items at the key's
// own indentation, so those items need to nest without being indented past it.
const EMPTY_KEY = /:[ \t]*(?:#.*)?$/;
// A block-scalar header: `|`/`>` with optional chomping (`-`/`+`) and explicit
// indentation indicator, standing alone as the line's value (so `re: a|b` and
// `filter: x > 0` are not mistaken for one). A trailing comment is legal there.
const BLOCK_HEADER = /(?:^|[:-])[ \t]*[|>][+-]?\d{0,2}[ \t]*(?:#.*)?$/;

function indentOf(line: string): number {
  return /^ */.exec(line)![0].length;
}

function dashLength(line: string, indent: number): number {
  return DASHES.exec(line.slice(indent))?.[0].length ?? 0;
}

/** Column at which a line's children must start to be nested under it. */
function childIndent(line: string, indent: number): number {
  return indent + Math.max(1, dashLength(line, indent));
}

/**
 * Column at which a *sequence item* may nest under this line, which is the
 * line's own column for a value-less mapping key (`key:` followed by `- x` at the
 * same indent — PyYAML's default). `Infinity` when no such exception applies:
 * a sequence item under a sequence item must be indented, else it is a sibling.
 */
function seqChildIndent(line: string, indent: number): number {
  const isKey = dashLength(line, indent) === 0 && EMPTY_KEY.test(line);
  return isKey ? indent : Infinity;
}

function isBlockHeader(line: string): boolean {
  return BLOCK_HEADER.test(line);
}

function node(text: string): YamlNode {
  return { text, indent: indentOf(text), children: [], block: [] };
}

/**
 * Split `yaml` into a forest of fold nodes.
 *
 * Trailing blank lines are dropped; interior blank lines are kept as leaves at
 * whatever level is open, so round-tripping the rendered text reproduces the
 * input.
 */
export function foldYaml(yaml: string): YamlNode[] {
  const lines = yaml.replace(/\n+$/, '').split('\n');
  const roots: YamlNode[] = [];
  // Open ancestors, outermost first, paired with the columns their children need
  // (`seqIndent` is the looser column a sequence item may use — see above).
  const stack: { node: YamlNode; childIndent: number; seqIndent: number }[] = [];

  const push = (n: YamlNode, childCol: number, seqCol: number) => {
    const parent = stack.length ? stack[stack.length - 1].node : null;
    (parent ? parent.children : roots).push(n);
    stack.push({ node: n, childIndent: childCol, seqIndent: seqCol });
  };

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Blank lines carry no indentation information — keep them where we are.
    if (BLANK.test(line)) {
      const n = node(line);
      if (stack.length) stack[stack.length - 1].node.children.push(n);
      else roots.push(n);
      continue;
    }

    const indent = indentOf(line);
    const isSeq = dashLength(line, indent) > 0;
    while (stack.length) {
      const top = stack[stack.length - 1];
      const need = isSeq ? Math.min(top.childIndent, top.seqIndent) : top.childIndent;
      if (indent >= need) break;
      stack.pop();
    }

    const n = node(line);
    push(n, childIndent(line, indent), seqChildIndent(line, indent));

    if (!isBlockHeader(line)) continue;

    // Consume the block body: everything indented past the header, plus any
    // blank lines inside it. Never tokenized, never folded further.
    while (i + 1 < lines.length) {
      const next = lines[i + 1];
      if (!BLANK.test(next) && indentOf(next) <= indent) break;
      n.block.push(next);
      i++;
    }
    // Blank lines that trail the block belong after it, not inside it: give
    // them back to the main loop.
    while (n.block.length && BLANK.test(n.block[n.block.length - 1])) {
      n.block.pop();
      i--;
    }
  }

  return roots;
}

/** Lines a node hides when collapsed: its block body plus all descendants. */
export function hiddenLineCount(n: YamlNode): number {
  let total = n.block.length;
  for (const c of n.children) total += 1 + hiddenLineCount(c);
  return total;
}
