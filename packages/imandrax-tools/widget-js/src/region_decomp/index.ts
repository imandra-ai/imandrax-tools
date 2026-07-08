// anywidget entry point for the treemap view (the primary region-decomposition
// widget). A thin adapter over the pure `drawTreemap`: pull the one-directional
// `data` traitlet off the model, render, and re-render when it changes.

import { drawTreemap } from './treemap';
import type { DrawInput } from './types';

interface Model {
  get(key: 'data'): DrawInput;
  on(event: 'change:data', cb: () => void): void;
}

export default {
  render({ model, el }: { model: Model; el: HTMLElement }) {
    const rerender = () => drawTreemap(el, model.get('data'));
    rerender();
    model.on('change:data', rerender);
  },
};
