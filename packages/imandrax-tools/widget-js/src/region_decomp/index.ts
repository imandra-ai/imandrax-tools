// anywidget entry point. A thin adapter over the pure `draw` function: pull the
// one-directional `data` traitlet off the model, render, and re-render whenever
// it changes. There is almost nothing to test here — the logic lives in draw.ts.

import { draw } from "./draw";
import type { DrawInput } from "./types";

interface Model {
  get(key: "data"): DrawInput;
  on(event: "change:data", cb: () => void): void;
}

export default {
  render({ model, el }: { model: Model; el: HTMLElement }) {
    const rerender = () => draw(el, model.get("data"));
    rerender();
    model.on("change:data", rerender);
  },
};
