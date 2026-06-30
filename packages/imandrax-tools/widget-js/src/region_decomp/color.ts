// Shared coloring for both views. Each top-level branch gets a stable Tableau
// hue; cells grow paler with depth, so a subtree reads as one color family and
// dark labels stay legible.

import { scaleOrdinal } from 'd3-scale';
import { schemeTableau10 } from 'd3-scale-chromatic';

import { labelPath, type Node } from './nodes';

const ROOT_COLOR = '#eceff1';
const MIN_TINT = 0.12; // depth-0 branch tint
const TINT_SPAN = 0.46; // extra tint added by the deepest level

export function makeColorFn(rootNode: Node): (node: Node) => string {
  const branchColor = scaleOrdinal(schemeTableau10);
  const maxDepth = Math.max(1, rootNode.height);
  return (node) => {
    if (node.depth === 0) return ROOT_COLOR;
    const top = node.ancestors().find((a) => a.depth === 1)!;
    const base = branchColor(labelPath(top));
    const t = MIN_TINT + TINT_SPAN * ((node.depth - 1) / maxDepth);
    return mixWhite(base, t);
  };
}

// Mix a `#rrggbb` color toward white. t=0 → base, t=1 → white.
function mixWhite(hex: string, t: number): string {
  const r = parseInt(hex.slice(1, 3), 16);
  const g = parseInt(hex.slice(3, 5), 16);
  const b = parseInt(hex.slice(5, 7), 16);
  const m = (v: number) => Math.round(v + (255 - v) * t);
  return `rgb(${m(r)}, ${m(g)}, ${m(b)})`;
}
