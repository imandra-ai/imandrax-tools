import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import { describe, expect, it } from "vitest";

import { drawTreemap } from "../src/region_decomp/treemap";

// Fixtures are the exact `RegionDecompWidget` input (the `data` traitlet list the
// Python side feeds the frontend), generated from real API output by
// scripts/gen_widget_input_fixtures.
function loadFixture(name) {
  // vitest runs with the package dir as cwd.
  const path = resolve(
    process.cwd(),
    `test/fixtures/inputs/${name}.widget_input.json`,
  );
  return JSON.parse(readFileSync(path, "utf8"));
}

const classify = loadFixture("decomp.simple.classify.iml");

// Find a rendered cell/tile by its `[label_path]` (the bold `-lp` span text).
function byLabelPath(el, labelPath) {
  for (const span of el.querySelectorAll(".imdx-rd-lp")) {
    if (span.textContent === labelPath)
      return span.closest(".imdx-rd-cell, .imdx-rd-tile");
  }
  return null;
}

const click = (node) =>
  node.dispatchEvent(new MouseEvent("click", { bubbles: true }));
const dblclick = (node) =>
  node.dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));

describe("region_decomp", () => {
  const render = (data) => {
    const el = document.createElement("div");
    drawTreemap(el, data);
    return el;
  };

  it("renders a tile for each visible region", () => {
    // classify is shallow (depth <= 3), so every non-root node is visible.
    expect(render(classify).querySelectorAll(".imdx-rd-tile").length).toBe(8);
  });

  it("tolerates an empty forest (decomposition error)", () => {
    const el = render([]);
    expect(el.querySelectorAll(".imdx-rd-tile").length).toBe(0);
    expect(el.querySelectorAll(".imdx-rd-crumb").length).toBe(1);
  });

  it("starts zoomed to the root with a single breadcrumb", () => {
    const crumbs = render(classify).querySelectorAll(".imdx-rd-crumb");
    expect(crumbs.length).toBe(1);
    expect(crumbs[0].textContent).toBe("root");
    expect(crumbs[0].classList.contains("-current")).toBe(true);
  });

  it("shows the region detail on single click", () => {
    const el = render(classify);
    click(byLabelPath(el, "1.1.1"));
    const detail = el.querySelector(".imdx-rd-detail").textContent;
    expect(detail).toContain("Invariant");
    expect(detail).toContain("x = 1");
  });

  it("zooms into a region on double click, extending the breadcrumb", () => {
    const el = render(classify);
    dblclick(byLabelPath(el, "1"));
    const crumbs = el.querySelectorAll(".imdx-rd-crumb");
    expect(crumbs.length).toBe(2);
    expect(crumbs[1].textContent).toBe("[1]");
    expect(crumbs[1].classList.contains("-current")).toBe(true);
    // now the tiles are [1]'s descendants; [1] itself is the zoom frame.
    expect(byLabelPath(el, "1.1")).not.toBeNull();
  });

  it("zooms back out when a breadcrumb ancestor is clicked", () => {
    const el = render(classify);
    dblclick(byLabelPath(el, "1"));
    click(el.querySelector(".imdx-rd-crumb")); // the "root" crumb
    expect(el.querySelectorAll(".imdx-rd-crumb").length).toBe(1);
    expect(byLabelPath(el, "1")).not.toBeNull();
  });
});
