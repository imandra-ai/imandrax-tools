// anywidget entry point for the task-artifact view. A thin adapter over the pure
// `drawTasks`: pull the one-directional `task_entries` traitlet off the model,
// render, and re-render when it changes.

import type { TaskData } from './types';
import { drawTasks } from './view';

interface Model {
  get(key: 'task_entries'): TaskData[];
  on(event: 'change:task_entries', cb: () => void): void;
}

export default {
  render({ model, el }: { model: Model; el: HTMLElement }) {
    const rerender = () => drawTasks(el, model.get('task_entries'));
    rerender();
    model.on('change:task_entries', rerender);
  },
};
