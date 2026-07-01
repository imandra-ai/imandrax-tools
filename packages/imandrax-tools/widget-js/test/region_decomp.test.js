import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';

import { describe, expect, it } from 'vitest';

import { drawIcicle } from '../src/region_decomp/icicle';
import { drawTreemap } from '../src/region_decomp/treemap';

// Fixtures are the exact `RegionDecompWidget` input (the list the Python side
// feeds the frontend), generated from real API output by scripts/gen_fixtures.py
// (.jsonc, with a leading `//` header). The views consume them as-is, so these
// tests exercise the actual data the widget gets.
function loadFixture(name) {
  // vitest runs with the package dir as cwd.
  const path = resolve(process.cwd(), `test/fixtures/region_decomp/${name}.widget_data.jsonc`);
  const raw = readFileSync(path, 'utf8').replace(/^\s*\/\/.*$/gm, '');
  return JSON.parse(raw);
}

const classify = loadFixture('classify');

// Find a rendered cell/tile by its `[label_path]` (the bold `-lp` span text).
function byLabelPath(el, labelPath) {
  for (const span of el.querySelectorAll('.imdx-rd-lp')) {
    if (span.textContent === labelPath) return span.closest('.imdx-rd-cell, .imdx-rd-tile');
  }
  return null;
}

const click = (node) => node.dispatchEvent(new MouseEvent('click', { bubbles: true }));
const dblclick = (node) => node.dispatchEvent(new MouseEvent('dblclick', { bubbles: true }));

describe('region_decomp — icicle', () => {
  const render = (data) => {
    const el = document.createElement('div');
    drawIcicle(el, data);
    return el;
  };

  it('renders one cell per node, including the synthetic root', () => {
    // classify forest: 8 nodes + synthetic "all regions" root.
    expect(render(classify).querySelectorAll('.imdx-rd-cell').length).toBe(9);
  });

  it('wraps the forest in a root cell spanning the full width on the top row', () => {
    const root = byLabelPath(render(classify), 'root');
    expect(root.style.top).toBe('0px');
    expect(root.style.left).toBe('0px');
    expect(root.style.width).toBe('720px');
  });

  it('tolerates an empty forest (decomposition error)', () => {
    // Just the synthetic root cell, no crash.
    expect(render([]).querySelectorAll('.imdx-rd-cell').length).toBe(1);
  });

  it('shows a placeholder in the detail pane before any selection', () => {
    expect(render(classify).querySelector('.imdx-rd-placeholder')).not.toBeNull();
  });

  it('renders constraint path, invariant and example on clicking a leaf', () => {
    const el = render(classify);
    click(byLabelPath(el, '1.1.1'));
    const detail = el.querySelector('.imdx-rd-detail').textContent;
    expect(detail).toContain('x <= y');
    expect(detail).toContain('Invariant');
    expect(detail).toContain('Example input');
    expect(detail).toContain('x = 1');
  });

  it('marks hovered node ancestors as on-path', () => {
    const el = render(classify);
    byLabelPath(el, '1.1.1').dispatchEvent(new MouseEvent('mouseenter', { bubbles: true }));
    // leaf + [1.1] + [1] + root = 4 cells on the path
    expect(el.querySelectorAll('.imdx-rd-cell.is-onpath').length).toBe(4);
  });

  it('classify leaf [1.1.1] detail snapshot', async () => {
    const el = render(classify);
    click(byLabelPath(el, '1.1.1'));
    await expect(el.querySelector('.imdx-rd-detail').innerHTML).toMatchFileSnapshot(
      './snapshots/classify-leaf-detail.html',
    );
  });
});

describe('region_decomp — treemap', () => {
  const render = (data) => {
    const el = document.createElement('div');
    drawTreemap(el, data);
    return el;
  };

  it('renders a tile for each visible region', () => {
    // classify is shallow (depth <= 3), so every non-root node is visible.
    expect(render(classify).querySelectorAll('.imdx-rd-tile').length).toBe(8);
  });

  it('tolerates an empty forest (decomposition error)', () => {
    const el = render([]);
    expect(el.querySelectorAll('.imdx-rd-tile').length).toBe(0);
    expect(el.querySelectorAll('.imdx-rd-crumb').length).toBe(1);
  });

  it('starts zoomed to the root with a single breadcrumb', () => {
    const crumbs = render(classify).querySelectorAll('.imdx-rd-crumb');
    expect(crumbs.length).toBe(1);
    expect(crumbs[0].textContent).toBe('root');
    expect(crumbs[0].classList.contains('-current')).toBe(true);
  });

  it('shows the region detail on single click', () => {
    const el = render(classify);
    click(byLabelPath(el, '1.1.1'));
    const detail = el.querySelector('.imdx-rd-detail').textContent;
    expect(detail).toContain('Invariant');
    expect(detail).toContain('x = 1');
  });

  it('zooms into a region on double click, extending the breadcrumb', () => {
    const el = render(classify);
    dblclick(byLabelPath(el, '1'));
    const crumbs = el.querySelectorAll('.imdx-rd-crumb');
    expect(crumbs.length).toBe(2);
    expect(crumbs[1].textContent).toBe('[1]');
    expect(crumbs[1].classList.contains('-current')).toBe(true);
    // now the tiles are [1]'s descendants; [1] itself is the zoom frame.
    expect(byLabelPath(el, '1.1')).not.toBeNull();
  });

  it('zooms back out when a breadcrumb ancestor is clicked', () => {
    const el = render(classify);
    dblclick(byLabelPath(el, '1'));
    click(el.querySelector('.imdx-rd-crumb')); // the "root" crumb
    expect(el.querySelectorAll('.imdx-rd-crumb').length).toBe(1);
    expect(byLabelPath(el, '1')).not.toBeNull();
  });
});
