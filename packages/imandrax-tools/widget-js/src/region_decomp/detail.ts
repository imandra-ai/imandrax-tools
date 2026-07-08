// Shared detail panel. Both views render the same panel for a clicked node:
// its constraint path, invariant, and concrete example. Every aggregate is read
// off the d3 hierarchy, so nothing here depends on pre-computed stats.

import { introducedConstraint, labelPath, type Node } from './nodes';
import { ROOT_CLASS } from './style';
import type { RegionStat } from './types';

export const PLACEHOLDER_HTML = `<p class="${ROOT_CLASS}-placeholder">Click a region to see its constraints and example.</p>`;

export function detailHtml(node: Node): string {
  const introduced = introducedConstraint(node);
  const parts: string[] = [];
  parts.push(`<h3>[${esc(labelPath(node))}]</h3>`);
  if (introduced) {
    parts.push(`<p class="sub"><code>${esc(introduced)}</code></p>`);
  }

  parts.push(`<div class="stats">${statsHtml(node)}</div>`);

  const constraints = node.data.constraints || [];
  if (constraints.length) {
    parts.push(`<div class="k">Constraint path</div>`);
    const lis = constraints
      .map((c) => {
        const cur = c === introduced ? ' class="cur"' : '';
        return `<li${cur}><code>${esc(c)}</code></li>`;
      })
      .join('');
    parts.push(`<ol>${lis}</ol>`);
  }

  const stat = node.data.region_stat;
  if (stat) {
    parts.push(`<div class="k">Invariant</div><pre>${esc(stat.invariant)}</pre>`);
    parts.push(`<div class="k">Example input</div><pre>${esc(fmtModel(stat.model))}</pre>`);
    parts.push(`<div class="k">Example output</div><pre>${esc(stat.model_eval)}</pre>`);
  }
  return parts.join('');
}

function statsHtml(node: Node): string {
  const stat = (label: string, value: number) => `<span>${label}: <b>${value}</b></span>`;
  const nChildren = node.children ? node.children.length : 0;
  return [
    stat('leaf regions', node.leaves().length),
    stat('direct children', nChildren),
    stat('descendants', node.descendants().length - 1),
  ]
    .filter(Boolean)
    .join('');
}

// `model` is a {var: value-string} map; render it as aligned lines. May be a
// bare string, or absent/null when the region carries no example input.
function fmtModel(model: RegionStat['model']): string {
  if (model == null) return '(no inputs)';
  if (typeof model !== 'object') return String(model);
  const entries = Object.entries(model);
  if (!entries.length) return '(no inputs)';
  return entries.map(([k, v]) => `${k} = ${v}`).join('\n');
}

// HTML-escape a value for safe interpolation into innerHTML.
export function esc(s: unknown): string {
  if (s == null) return '';
  return String(s)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}
