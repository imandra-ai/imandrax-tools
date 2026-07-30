import { describe, expect, it, vi } from "vitest";

import { drawStacked } from "../src/common/stack";

// `drawStacked` is what lets a partly-renderable result show whole: the native
// panel covers what it understands, and the surrounding fields ride along as
// folded YAML above and below it. The contract worth pinning down is which slots
// render, in what order, and that the native `draw*` functions -- all of which
// wipe their target -- never receive the widget root itself.

const YAML = "eval_res:\n  errors: []\n";
const OTHER = "diagnostics:\n  - kind: unused\n";

// Stands in for drawTasks / drawTreemap: marks whatever element it is handed.
function fakeMain(el) {
  el.classList.add("fake-main");
  el.textContent = "native panel";
}

function render(input) {
  const el = document.createElement("div");
  drawStacked(el, { pre: "", post: "", main: fakeMain, hasMain: true, ...input });
  return el;
}

// The rendered slots in document order; `style` and the placeholder are not slots.
// A jsonable slot reports its YAML body, whitespace-collapsed: each line is its
// own element, and the fidelity of the YAML rendering itself is jsonable.test.js's
// business, not this file's. `drawJsonable` also injects a <style> and a toolbar,
// neither of which is content.
function slots(el) {
  return [...el.children]
    .filter((c) => c.tagName !== "STYLE" && !c.className.includes("-placeholder"))
    .map((c) => {
      if (c.classList.contains("fake-main")) return "main";
      const doc = c.querySelector(".imdx-jsonable-doc").cloneNode(true);
      // The folded-line counts ("…1 line") are chrome on the summary, not content.
      for (const count of doc.querySelectorAll(".imdx-jsonable-count")) count.remove();
      return doc.textContent.replace(/\s+/g, " ").trim();
    });
}

describe("stack", () => {
  it("orders the slots pre, main, post", () => {
    expect(slots(render({ pre: YAML, post: OTHER }))).toEqual([
      "eval_res: errors: []",
      "main",
      "diagnostics: - kind: unused",
    ]);
  });

  it("renders only the native panel when pre and post are empty", () => {
    expect(slots(render({}))).toEqual(["main"]);
  });

  it("drops the native panel when there is nothing to lay out", () => {
    const el = render({ pre: YAML, hasMain: false });
    expect(el.querySelector(".fake-main")).toBeNull();
    expect(slots(el)).toEqual(["eval_res: errors: []"]);
  });

  it("never calls the native draw when hasMain is false", () => {
    const main = vi.fn();
    render({ pre: YAML, main, hasMain: false });
    expect(main).not.toHaveBeenCalled();
  });

  it("treats whitespace-only yaml as absent", () => {
    expect(slots(render({ pre: "  \n\n", post: "\t" }))).toEqual(["main"]);
  });

  it("draws each slot into its own child, never the root", () => {
    // The `draw*` functions all clear their target and stamp a root class on it,
    // so sharing the root would let one slot erase the others.
    const el = render({ pre: YAML, post: OTHER });
    expect(el.classList.contains("fake-main")).toBe(false);
    expect(el.querySelector(".fake-main").parentElement).toBe(el);
    expect(el.querySelectorAll(".imdx-jsonable").length).toBe(2);
  });

  it("shows a placeholder when every slot is empty", () => {
    const el = render({ hasMain: false });
    expect(el.querySelector(".imdx-stack-placeholder").textContent).toBe(
      "Nothing to show.",
    );
  });

  it("clears previous output when re-rendered into the same element", () => {
    const el = render({ pre: YAML });
    drawStacked(el, { pre: OTHER, post: "", main: fakeMain, hasMain: false });
    expect(el.querySelector(".fake-main")).toBeNull();
    expect(slots(el)).toEqual(["diagnostics: - kind: unused"]);
  });
});
