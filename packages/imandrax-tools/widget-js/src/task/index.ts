// anywidget entry point for the task-artifact view. A thin adapter over the pure
// `drawTasks`: pull the one-directional `tasks` traitlet off the model, render,
// and re-render when it changes.

import type { TaskData } from './types';
import { drawTasks } from './view';

interface Model {
  get(key: 'tasks'): TaskData[];
  on(event: 'change:tasks', cb: () => void): void;
}

export default {
  render({ model, el }: { model: Model; el: HTMLElement }) {
    const rerender = () => drawTasks(el, model.get('tasks'));
    rerender();
    model.on('change:tasks', rerender);
  },
};
