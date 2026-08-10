import { describe, expect, it } from "vitest";

import decompAdapter from "../src/region_decomp/index";
import taskAdapter from "../src/task/index";

// The anywidget adapters: which traitlets each reads, and when the native panel
// is dropped in favour of the `pre` / `post` YAML. The panels' own rendering is
// covered by stack.test.js and jsonable.test.js.

// Minimal stand-in for the anywidget model: `get` plus change subscriptions.
function mockModel(state) {
  const listeners = {};
  return {
    get: (key) => state[key],
    on: (event, cb) => ((listeners[event] ||= []).push(cb), undefined),
    set(key, value) {
      state[key] = value;
      for (const cb of listeners[`change:${key}`] || []) cb();
    },
  };
}

function mount(adapter, state) {
  const el = document.createElement("div");
  const model = mockModel(state);
  adapter.render({ model, el });
  return { el, model };
}

const TASK = { kind: "verify", id: "1", artifacts: [] };
const GROUP = { label_path: ["a"], constraints: [], weight: 1, region_stat: null, children: [] };
const PRE = "eval_res:\n  errors: []\n";

const hasJsonable = (el) => el.querySelectorAll(".imdx-jsonable").length;

describe("task adapter", () => {
  const state = (over) => ({ task_entries: null, pre: "", post: "", ...over });

  it("renders the tasks panel when there are entries", () => {
    const { el } = mount(taskAdapter, state({ task_entries: [TASK] }));
    expect(el.querySelectorAll(".imdx-task-task").length).toBe(1);
    expect(hasJsonable(el)).toBe(0);
  });

  it("renders pre and post alongside the tasks panel", () => {
    const { el } = mount(taskAdapter, state({ task_entries: [TASK], pre: PRE, post: PRE }));
    expect(el.querySelectorAll(".imdx-task-task").length).toBe(1);
    expect(hasJsonable(el)).toBe(2);
  });

  it("drops the tasks panel when task_entries is null", () => {
    const { el } = mount(taskAdapter, state({ pre: PRE }));
    expect(el.querySelector(".imdx-task-task")).toBeNull();
    expect(el.querySelector(".imdx-task-placeholder")).toBeNull();
    expect(hasJsonable(el)).toBe(1);
  });

  it("keeps the panel for an empty array, which reports no tasks", () => {
    // [] and null differ: an eval that ran and produced nothing says so.
    const { el } = mount(taskAdapter, state({ task_entries: [], pre: PRE }));
    expect(el.querySelector(".imdx-task-task")).toBeNull();
    expect(el.querySelector(".imdx-task-placeholder").textContent).toBe("No tasks.");
    expect(hasJsonable(el)).toBe(1);
  });

  it("re-renders when pre changes", () => {
    const { el, model } = mount(taskAdapter, state({ task_entries: [TASK] }));
    expect(hasJsonable(el)).toBe(0);
    model.set("pre", PRE);
    expect(hasJsonable(el)).toBe(1);
    expect(el.querySelectorAll(".imdx-task-task").length).toBe(1);
  });
});

describe("region_decomp adapter", () => {
  const state = (over) => ({ data: null, pre: "", post: "", ...over });

  it("renders the treemap when there are region groups", () => {
    const { el } = mount(decompAdapter, state({ data: [GROUP] }));
    expect(el.querySelector(".imdx-rd-tiles")).not.toBeNull();
    expect(el.querySelector(".imdx-rd-placeholder").textContent).not.toBe("No regions.");
    expect(hasJsonable(el)).toBe(0);
  });

  it("drops the treemap when data is null", () => {
    // What an errored decomposition looks like: no forest, result in `pre`.
    const { el } = mount(decompAdapter, state({ pre: PRE }));
    expect(el.querySelector(".imdx-rd-tiles")).toBeNull();
    expect(hasJsonable(el)).toBe(1);
  });

  it("keeps the treemap for an empty array, which reports no regions", () => {
    const { el } = mount(decompAdapter, state({ data: [], pre: PRE }));
    expect(el.querySelectorAll(".imdx-rd-tile").length).toBe(0);
    expect(el.querySelector(".imdx-rd-tiles .imdx-rd-placeholder").textContent).toBe(
      "No regions.",
    );
    expect(hasJsonable(el)).toBe(1);
  });

  it("re-renders when data arrives", () => {
    const { el, model } = mount(decompAdapter, state({ pre: PRE }));
    expect(el.querySelector(".imdx-rd-tiles")).toBeNull();
    model.set("data", [GROUP]);
    expect(el.querySelector(".imdx-rd-tiles")).not.toBeNull();
  });
});
