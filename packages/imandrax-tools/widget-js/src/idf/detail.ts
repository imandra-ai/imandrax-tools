// The right-hand detail panel. It is dual-purpose: clicking a region node shows
// that region's body (constraint path, invariant, concrete example); clicking a
// step band shows that step's request metadata (guard/target IML, basis, ...).

import { ROOT_CLASS } from './style';
import type { RegionNodeView, StepView } from './types';

export const PLACEHOLDER_HTML = `<p class="${ROOT_CLASS}-placeholder">Click a region node for its body, or a step band for its details.</p>`;

// Region-node body view
// --------------------

function nodeKind(r: RegionNodeView): 'root' | 'leaf' | 'interior' {
  if (r.is_root) return 'root';
  return r.is_leaf ? 'leaf' : 'interior';
}

export function nodeDetailHtml(r: RegionNodeView): string {
  const kind = nodeKind(r);
  const parts: string[] = [];
  parts.push(
    `<h3>#${r.id}<span class="${ROOT_CLASS}-tag -${kind}">${kind}</span></h3>`,
  );
  parts.push(`<p class="sub">step ${r.step_idx} · <code>${esc(r.raw_id)}</code></p>`);

  if (r.constraints.length) {
    parts.push(`<div class="k">Constraints</div>`);
    const lis = r.constraints.map((c) => `<li><code>${esc(c)}</code></li>`).join('');
    parts.push(`<ol>${lis}</ol>`);
  }

  if (r.invariant != null) {
    parts.push(`<div class="k">Invariant</div><pre>${esc(r.invariant)}</pre>`);
  }
  parts.push(`<div class="k">Example input</div><pre>${esc(fmtModel(r.model))}</pre>`);
  if (r.model_eval != null) {
    parts.push(`<div class="k">Example output</div><pre>${esc(r.model_eval)}</pre>`);
  }
  return parts.join('');
}

// Step metadata view
// --------------------

export function stepDetailHtml(step: StepView): string {
  const parts: string[] = [];
  parts.push(`<h3>Step ${step.step_idx}</h3>`);
  parts.push(`<p class="sub">${esc(step.message)}</p>`);

  parts.push(`<div class="stats">`);
  parts.push(`<span>regions: <b>${step.n_regions}</b></span>`);
  parts.push(`<span>target: <b><code>${esc(step.name)}</code></b></span>`);
  parts.push(`</div>`);

  parts.push(`<div class="k">Assuming</div><pre>${esc(step.assuming)}</pre>`);
  const basis = step.basis.length ? step.basis.join(', ') : '(none)';
  parts.push(`<div class="k">Basis</div><pre>${esc(basis)}</pre>`);
  if (step.merge_with != null) {
    parts.push(`<div class="k">Merged with</div><pre>${esc(step.merge_with)}</pre>`);
  }
  parts.push(`<div class="k">Guard</div><pre>${esc(step.guard_iml.trim())}</pre>`);
  parts.push(`<div class="k">Target</div><pre>${esc(step.target_iml.trim())}</pre>`);
  return parts.join('');
}

// Helpers
// --------------------

// `model` is a {var: value-string} map; render it as aligned lines. May be a
// bare string, or absent/empty when the region carries no example input.
function fmtModel(model: RegionNodeView['model']): string {
  if (model == null) return '(no inputs)';
  if (typeof model !== 'object') return String(model);
  const entries = Object.entries(model);
  if (!entries.length) return '(no inputs)';
  return entries.map(([k, v]) => `${k} = ${v}`).join('\n');
}

export function esc(s: unknown): string {
  if (s == null) return '';
  return String(s)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}
