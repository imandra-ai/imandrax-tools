// Task-artifact view: a stack of collapsible tasks, each holding collapsible
// artifacts rendered as escaped, scrollable <pre> text.
//
// `drawTasks(el, tasks)` builds the DOM, wires interaction, and returns nothing.

import { highlightRepr } from './highlight';
import { ROOT_CLASS, TASK_STYLE } from './style';
import type { Artifact, TaskData } from './types';

function makeArtifact(art: Artifact): HTMLElement {
  const details = document.createElement('details');
  details.className = `${ROOT_CLASS}-art`;
  details.open = true;

  const summary = document.createElement('summary');
  summary.className = `${ROOT_CLASS}-summary`;
  const kind = document.createElement('span');
  kind.className = `${ROOT_CLASS}-art-kind`;
  kind.textContent = art.kind;
  summary.appendChild(kind);

  const meta = document.createElement('span');
  meta.className = `${ROOT_CLASS}-meta`;
  meta.textContent = `${art.text.length.toLocaleString()} chars`;
  summary.appendChild(meta);

  const copy = document.createElement('button');
  copy.className = `${ROOT_CLASS}-copy`;
  copy.type = 'button';
  copy.textContent = 'copy';
  copy.addEventListener('click', (e) => {
    e.preventDefault();
    e.stopPropagation();
    navigator.clipboard?.writeText(art.text).then(() => {
      copy.textContent = 'copied';
      setTimeout(() => (copy.textContent = 'copy'), 1200);
    });
  });
  summary.appendChild(copy);
  details.appendChild(summary);

  const scroll = document.createElement('div');
  scroll.className = `${ROOT_CLASS}-scroll`;
  const pre = document.createElement('pre');
  pre.className = `${ROOT_CLASS}-pre`;
  pre.innerHTML = highlightRepr(art.text); // tokens are HTML-escaped by highlightRepr
  scroll.appendChild(pre);
  details.appendChild(scroll);
  return details;
}

function makeTask(task: TaskData): HTMLElement {
  const details = document.createElement('details');
  details.className = `${ROOT_CLASS}-task`;
  details.open = true;

  const summary = document.createElement('summary');
  summary.className = `${ROOT_CLASS}-summary`;

  const kind = document.createElement('span');
  kind.className = `${ROOT_CLASS}-kind`;
  kind.textContent = task.kind;
  summary.appendChild(kind);

  if (task.id) {
    const id = document.createElement('span');
    id.className = `${ROOT_CLASS}-id`;
    id.textContent = task.id;
    summary.appendChild(id);
  }

  const meta = document.createElement('span');
  meta.className = `${ROOT_CLASS}-meta`;
  const n = task.artifacts.length;
  meta.textContent = `${n} artifact${n === 1 ? '' : 's'}`;
  summary.appendChild(meta);
  details.appendChild(summary);

  const body = document.createElement('div');
  body.className = `${ROOT_CLASS}-body`;
  for (const art of task.artifacts) body.appendChild(makeArtifact(art));
  details.appendChild(body);
  return details;
}

export function drawTasks(el: HTMLElement, tasks: TaskData[]): void {
  el.innerHTML = '';
  el.classList.add(ROOT_CLASS);

  const style = document.createElement('style');
  style.textContent = TASK_STYLE;
  el.appendChild(style);

  if (!tasks || tasks.length === 0) {
    const empty = document.createElement('div');
    empty.className = `${ROOT_CLASS}-placeholder`;
    empty.textContent = 'No tasks.';
    el.appendChild(empty);
    return;
  }
  for (const task of tasks) el.appendChild(makeTask(task));
}
