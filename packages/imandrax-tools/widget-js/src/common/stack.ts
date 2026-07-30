// Vertical stacking of an optional YAML preamble, a widget's own view, and an
// optional YAML appendix.
//
// This is how a result that is only *partly* renderable by a native view gets
// shown whole: the native panel covers what it understands (tasks, a region
// forest), and the surrounding fields (`eval_res`, `diagnostics`) ride along as
// folded YAML via `drawJsonable`. Each slot is independent, so a failed result
// whose native panel would be empty degrades to just the YAML rather than to a
// text/plain fallback.
//
// Labels are deliberately absent: the YAML the Python side hands over is a
// mapping whose own top-level keys already name each field.

import { drawJsonable } from '../jsonable/view';

export const ROOT_CLASS = 'imdx-stack';

export const STACK_STYLE = `
.${ROOT_CLASS} { display: flex; flex-direction: column; gap: 8px; box-sizing: border-box; }
.${ROOT_CLASS}-placeholder { font-family: ui-sans-serif, system-ui, sans-serif;
  font-size: 12px; color: #9aa1a9; font-style: italic; padding: 10px;
  border: 1px solid #d8dde2; border-radius: 6px; background: #fff; }
`;

export interface StackInput {
  /** YAML shown above the native panel; blank or whitespace renders nothing. */
  pre: string;
  /** YAML shown below the native panel; blank or whitespace renders nothing. */
  post: string;
  /**
   * Draws the native panel into a fresh child element. Called only when
   * `hasMain` holds, since the `draw*` functions have no useful rendering for
   * empty input (`drawTreemap` would lay out an empty treemap).
   */
  main: (el: HTMLElement) => void;
  hasMain: boolean;
}

export function drawStacked(el: HTMLElement, input: StackInput): void {
  el.innerHTML = '';
  el.classList.add(ROOT_CLASS);

  const style = document.createElement('style');
  style.textContent = STACK_STYLE;
  el.appendChild(style);

  // Every slot draws into its own child: the `draw*` functions all wipe their
  // target and stamp a root class on it, so none of them may touch `el` itself.
  const section = (): HTMLElement => {
    const child = document.createElement('div');
    el.appendChild(child);
    return child;
  };

  const hasPre = Boolean(input.pre && input.pre.trim());
  const hasPost = Boolean(input.post && input.post.trim());

  if (hasPre) drawJsonable(section(), input.pre);
  if (input.hasMain) input.main(section());
  if (hasPost) drawJsonable(section(), input.post);

  if (!hasPre && !hasPost && !input.hasMain) {
    const empty = document.createElement('div');
    empty.className = `${ROOT_CLASS}-placeholder`;
    empty.textContent = 'Nothing to show.';
    el.appendChild(empty);
  }
}
