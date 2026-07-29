// The jsonable view: a toolbar plus the YAML document rendered line-by-line,
// where any line with nested content becomes a <details> the reader can fold.
//
// `drawJsonable(el, yaml, label?)` builds the DOM, wires interaction, and returns
// nothing. Rendering never rewrites the text it is given, so what is shown is
// also what the copy button yields.

import { foldYaml, hiddenLineCount, type YamlNode } from './fold';
import { highlightBlockLine, highlightLine } from './highlight';
import { JSONABLE_STYLE, ROOT_CLASS } from './style';

/** How deep to leave folds open initially; deeper levels start collapsed. */
const OPEN_DEPTH = 3;

function gutter(): HTMLElement {
  const arrow = document.createElement('span');
  arrow.className = `${ROOT_CLASS}-arrow`;
  return arrow;
}

function lineText(html: string): HTMLElement {
  const text = document.createElement('span');
  text.className = `${ROOT_CLASS}-text`;
  text.innerHTML = html; // tokens are HTML-escaped by highlightLine
  return text;
}

function makeBlock(lines: string[]): HTMLElement {
  const pre = document.createElement('div');
  pre.className = `${ROOT_CLASS}-block`;
  pre.innerHTML = lines.map(highlightBlockLine).join('\n');
  return pre;
}

function makeNode(node: YamlNode, depth: number): HTMLElement {
  const foldable = node.children.length > 0 || node.block.length > 0;

  if (!foldable) {
    const line = document.createElement('div');
    line.className = `${ROOT_CLASS}-line`;
    line.append(gutter(), lineText(highlightLine(node.text)));
    return line;
  }

  const details = document.createElement('details');
  details.className = `${ROOT_CLASS}-fold`;
  details.open = depth < OPEN_DEPTH;

  const summary = document.createElement('summary');
  summary.className = `${ROOT_CLASS}-line`;
  summary.append(gutter(), lineText(highlightLine(node.text)));

  // Shown only while collapsed (hidden via CSS when open).
  const count = document.createElement('span');
  count.className = `${ROOT_CLASS}-count`;
  const n = hiddenLineCount(node);
  count.textContent = `…${n} line${n === 1 ? '' : 's'}`;
  summary.appendChild(count);
  details.appendChild(summary);

  if (node.block.length) details.appendChild(makeBlock(node.block));
  for (const child of node.children) details.appendChild(makeNode(child, depth + 1));
  return details;
}

export function drawJsonable(el: HTMLElement, yaml: string, label = ''): void {
  el.innerHTML = '';
  el.classList.add(ROOT_CLASS);

  const style = document.createElement('style');
  style.textContent = JSONABLE_STYLE;
  el.appendChild(style);

  if (!yaml || !yaml.trim()) {
    const empty = document.createElement('div');
    empty.className = `${ROOT_CLASS}-placeholder`;
    empty.textContent = 'Nothing to show.';
    el.appendChild(empty);
    return;
  }

  const nodes = foldYaml(yaml);

  const doc = document.createElement('div');
  doc.className = `${ROOT_CLASS}-doc`;
  for (const node of nodes) doc.appendChild(makeNode(node, 0));

  const scroll = document.createElement('div');
  scroll.className = `${ROOT_CLASS}-scroll`;
  scroll.appendChild(doc);

  el.appendChild(makeBar(doc, yaml, label));
  el.appendChild(scroll);
}

function makeBar(doc: HTMLElement, yaml: string, label: string): HTMLElement {
  const bar = document.createElement('div');
  bar.className = `${ROOT_CLASS}-bar`;

  if (label) {
    const name = document.createElement('span');
    name.className = `${ROOT_CLASS}-label`;
    name.textContent = label;
    bar.appendChild(name);
  }

  const meta = document.createElement('span');
  meta.className = `${ROOT_CLASS}-meta`;
  const lines = yaml.replace(/\n+$/, '').split('\n').length;
  meta.textContent = `${lines.toLocaleString()} line${lines === 1 ? '' : 's'}`;
  bar.appendChild(meta);

  const actions = document.createElement('div');
  actions.className = `${ROOT_CLASS}-actions`;

  const button = (text: string, onClick: () => void) => {
    const b = document.createElement('button');
    b.className = `${ROOT_CLASS}-btn`;
    b.type = 'button';
    b.textContent = text;
    b.addEventListener('click', onClick);
    actions.appendChild(b);
    return b;
  };

  const setAll = (open: boolean) => {
    for (const d of doc.querySelectorAll('details')) d.open = open;
  };
  button('expand all', () => setAll(true));
  button('collapse all', () => setAll(false));

  const copy = button('copy', () => {
    navigator.clipboard?.writeText(yaml).then(() => {
      copy.textContent = 'copied';
      setTimeout(() => (copy.textContent = 'copy'), 1200);
    });
  });

  bar.appendChild(actions);
  return bar;
}
