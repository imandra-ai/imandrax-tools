import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';

import { describe, expect, it } from 'vitest';

import { draw } from '../src/region_decomp/draw.js';
import { toForest } from '../src/region_decomp/to_forest.js';

// Fixtures are raw EnrichedDecomposeRes dumps generated from real API output by
// scripts/gen_fixtures.py (.jsonc, with a leading `//` header). We enrich them
// via toForest exactly as the host does, so these tests exercise the actual
// forest the widget is handed.
function loadFixture(name) {
  // vitest runs with the package dir as cwd.
  const path = resolve(process.cwd(), `test/fixtures/region_decomp/${name}.enriched_decompose_res.jsonc`);
  const raw = readFileSync(path, 'utf8').replace(/^\s*\/\/.*$/gm, '');
  return toForest(JSON.parse(raw));
}

const classify = loadFixture('classify');

function render(regionGroups) {
  const el = document.createElement('div');
  draw(el, regionGroups);
  return el;
}

// Find a rendered cell by its `[label_path]` (the bold span text).
function cellByLabelPath(el, labelPath) {
  const lps = el.querySelectorAll('.imdx-rd-lp');
  for (const span of lps) {
    if (span.textContent === labelPath) return span.closest('.imdx-rd-cell');
  }
  return null;
}

describe('region_decomp — structure', () => {
  it('renders one cell per node, including the synthetic root', () => {
    // classify forest: 8 nodes + synthetic "all regions" root.
    expect(render(classify).querySelectorAll('.imdx-rd-cell').length).toBe(9);
  });

  it('wraps the forest in a root cell spanning the full width on the top row', () => {
    const root = cellByLabelPath(render(classify), 'root');
    expect(root.style.top).toBe('0px');
    expect(root.style.left).toBe('0px');
    expect(root.style.width).toBe('720px');
  });

  it('tolerates an empty forest (decomposition error)', () => {
    const el = render([]);
    // Just the synthetic root cell, no crash.
    expect(el.querySelectorAll('.imdx-rd-cell').length).toBe(1);
  });

  it('shows a placeholder in the detail pane before any selection', () => {
    expect(render(classify).querySelector('.imdx-rd-placeholder')).not.toBeNull();
  });
});

describe('region_decomp — interaction', () => {
  it('renders constraint path, invariant and example on clicking a leaf', () => {
    const el = render(classify);
    cellByLabelPath(el, '1.1.1').dispatchEvent(new MouseEvent('click', { bubbles: true }));
    const detail = el.querySelector('.imdx-rd-detail').textContent;
    expect(detail).toContain('x <= y');
    expect(detail).toContain('Invariant');
    expect(detail).toContain('Example input');
    expect(detail).toContain('x = 1');
  });

  it('marks hovered node ancestors as on-path', () => {
    const el = render(classify);
    cellByLabelPath(el, '1.1.1').dispatchEvent(new MouseEvent('mouseenter', { bubbles: true }));
    // leaf + [1.1] + [1] + root = 4 cells on the path
    expect(el.querySelectorAll('.imdx-rd-cell.is-onpath').length).toBe(4);
  });
});

describe('region_decomp — detail panel snapshot', () => {
  it('classify leaf [1.1.1]', async () => {
    const el = render(classify);
    cellByLabelPath(el, '1.1.1').dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await expect(el.querySelector('.imdx-rd-detail').innerHTML).toMatchFileSnapshot(
      './snapshots/classify-leaf-detail.html',
    );
  });
});
