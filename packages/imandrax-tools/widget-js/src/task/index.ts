// anywidget entry point for the task-artifact view. A thin adapter over the pure
// `drawTasks`, stacked between the optional `pre` / `post` YAML panels: pull the
// one-directional traitlets off the model, render, and re-render when any change.
//
// A null `task_entries` drops the tasks panel, leaving a widget that is only its
// `pre` / `post` slots -- how a failed eval renders. An empty array still draws
// the panel, whose own "No tasks." reports that there is nothing in it; note that
// `nb_hooks` never sends `[]`, collapsing it to null, so that only happens for a
// widget constructed directly.

import { drawStacked } from '../common/stack';
import type { TaskData } from './types';
import { drawTasks } from './view';

type Key = 'task_entries' | 'pre' | 'post';

interface Model {
  get(key: 'task_entries'): TaskData[] | null;
  get(key: 'pre' | 'post'): string;
  on(event: `change:${Key}`, cb: () => void): void;
}

const KEYS: Key[] = ['task_entries', 'pre', 'post'];

export default {
  render({ model, el }: { model: Model; el: HTMLElement }) {
    const rerender = () => {
      const tasks = model.get('task_entries');
      drawStacked(el, {
        pre: model.get('pre'),
        post: model.get('post'),
        main: (target) => drawTasks(target, tasks ?? []),
        hasMain: tasks != null,
      });
    };
    rerender();
    for (const key of KEYS) model.on(`change:${key}`, rerender);
  },
};
