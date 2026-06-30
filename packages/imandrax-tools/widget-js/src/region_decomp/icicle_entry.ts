// anywidget entry point for the icicle view. Mirror of index.ts but mounting
// `drawIcicle` instead of the treemap.

import { drawIcicle } from './icicle';
import type { DrawInput } from './types';

interface Model {
  get(key: 'data'): DrawInput;
  on(event: 'change:data', cb: () => void): void;
}

export default {
  render({ model, el }: { model: Model; el: HTMLElement }) {
    const rerender = () => drawIcicle(el, model.get('data'));
    rerender();
    model.on('change:data', rerender);
  },
};
