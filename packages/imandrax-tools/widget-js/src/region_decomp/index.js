// anywidget entry point. A thin adapter over the pure `draw` function: pull the
// one-directional `data` traitlet off the model, render, and re-render whenever
// it changes. There is almost nothing to test here — the logic lives in draw.js.

import { draw } from './draw.js';
import { toForest } from './to_forest.js';

export default {
  render({ model, el }) {
    // `data` is the raw EnrichedDecomposeRes (or its region-group list); enrich
    // it into the forest shape `draw` consumes. See to_forest.js.
    const rerender = () => draw(el, toForest(model.get('data')));
    rerender();
    model.on('change:data', rerender);
  },
};
