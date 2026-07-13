// anywidget entry point for the IDF graph view. A thin adapter over the pure
// `drawGraph`: pull the one-directional `data` traitlet (a serialized `View`)
// off the model, render, and re-render when it changes.

import { drawGraph } from './graph';
import type { DrawInput } from './types';

interface Model {
  get(key: 'data'): DrawInput;
  on(event: 'change:data', cb: () => void): void;
}

export default {
  render({ model, el }: { model: Model; el: HTMLElement }) {
    const rerender = () => drawGraph(el, model.get('data'));
    rerender();
    model.on('change:data', rerender);
  },
};
