// anywidget entry point for the treemap view (the primary region-decomposition
// widget). A thin adapter over the pure `drawTreemap`, stacked between the
// optional `pre` / `post` YAML panels: pull the one-directional traitlets off the
// model, render, and re-render when any change.
//
// A null `data` drops the treemap, leaving a widget that is only its `pre` /
// `post` slots -- how a decomposition that errored renders. An empty array still
// draws the treemap, whose own "No regions." says it ran and found none.

import { drawStacked } from '../common/stack';
import { drawTreemap } from './treemap';
import type { DrawInput } from './types';

type Key = 'data' | 'pre' | 'post';

interface Model {
  get(key: 'data'): DrawInput;
  get(key: 'pre' | 'post'): string;
  on(event: `change:${Key}`, cb: () => void): void;
}

const KEYS: Key[] = ['data', 'pre', 'post'];

export default {
  render({ model, el }: { model: Model; el: HTMLElement }) {
    const rerender = () => {
      const data = model.get('data');
      drawStacked(el, {
        pre: model.get('pre'),
        post: model.get('post'),
        main: (target) => drawTreemap(target, data),
        hasMain: data != null,
      });
    };
    rerender();
    for (const key of KEYS) model.on(`change:${key}`, rerender);
  },
};
