// anywidget entry point for the jsonable view: the general-purpose front end for
// any `JSONValue`, which the Python side hands over as a YAML string. A thin
// adapter over the pure `drawJsonable` -- pull the one-directional `yaml_str` /
// `label` traitlets off the model, render, and re-render when either changes.

import { drawJsonable } from './view';

interface Model {
  get(key: 'yaml_str' | 'label'): string;
  on(event: 'change:yaml_str' | 'change:label', cb: () => void): void;
}

export default {
  render({ model, el }: { model: Model; el: HTMLElement }) {
    const rerender = () => drawJsonable(el, model.get('yaml_str'), model.get('label'));
    rerender();
    model.on('change:yaml_str', rerender);
    model.on('change:label', rerender);
  },
};
