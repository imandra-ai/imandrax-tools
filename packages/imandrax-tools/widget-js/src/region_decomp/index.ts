// anywidget entry point for the treemap view (the primary region-decomposition
// widget). A thin adapter over the pure `drawTreemap`, stacked between the
// optional `pre` / `post` YAML panels: pull the one-directional traitlets off the
// model, render, and re-render when any change.
//
// `title`, when set, labels the treemap's root breadcrumb in place of "root".
// `collapsed` folds the treemap down to its breadcrumb bar. A change to it goes
// through the drawn treemap's handle rather than a redraw, which would lose the
// zoom and the picked region; clicking the bar folds it too, without writing
// back to the model.
//
// A null `data` drops the treemap, leaving a widget that is only its `pre` /
// `post` slots -- how a decomposition that errored renders. An empty array still
// draws the treemap, whose own "No regions." reports that it holds none; note that
// `nb_hooks` never sends `[]`, collapsing it to null, so that only happens for a
// widget constructed directly.

import { drawStacked } from '../common/stack';
import { drawTreemap } from './treemap';
import type { DrawInput, TreemapHandle } from './types';

type Key = 'data' | 'title' | 'pre' | 'post';

interface Model {
  get(key: 'data'): DrawInput;
  get(key: 'title' | 'pre' | 'post'): string;
  get(key: 'collapsed'): boolean;
  on(event: `change:${Key | 'collapsed'}`, cb: () => void): void;
}

const KEYS: Key[] = ['data', 'title', 'pre', 'post'];

export default {
  render({ model, el }: { model: Model; el: HTMLElement }) {
    let handle: TreemapHandle | null = null;
    const rerender = () => {
      const data = model.get('data');
      handle = null;
      drawStacked(el, {
        pre: model.get('pre'),
        post: model.get('post'),
        main: (target) => {
          handle = drawTreemap(target, data, {
            title: model.get('title'),
            collapsed: model.get('collapsed'),
          });
        },
        hasMain: data != null,
      });
    };
    rerender();
    for (const key of KEYS) model.on(`change:${key}`, rerender);
    model.on('change:collapsed', () => handle?.setCollapsed(model.get('collapsed')));
  },
};
