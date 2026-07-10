import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import { describe, expect, it } from "vitest";

import { drawGraph } from "../src/idf/graph";

// Fixtures are the exact `IDFWidget` input (the `data` traitlet, a serialized
// `View`), generated from real API output by scripts/gen_widget_input_fixtures.
function loadFixture(name) {
  const path = resolve(
    process.cwd(),
    `test/fixtures/inputs/${name}.widget_input.json`,
  );
  return JSON.parse(readFileSync(path, "utf8"));
}

const choose = loadFixture("idf.choose.iml");

const click = (node) =>
  node.dispatchEvent(new MouseEvent("click", { bubbles: true }));

// Node cards select via pointerdown+pointerup (a no-move press == a click).
const tap = (node) => {
  node.dispatchEvent(new Event("pointerdown", { bubbles: true }));
  node.dispatchEvent(new Event("pointerup", { bubbles: true }));
};

// Find a rendered node card by its `#id` label.
function byNodeId(el, id) {
  for (const span of el.querySelectorAll(".imdx-idf-nid")) {
    if (span.textContent === `#${id}` || span.textContent === id)
      return span.closest(".imdx-idf-node");
  }
  return null;
}

const render = (data) => {
  const el = document.createElement("div");
  drawGraph(el, data);
  return el;
};

describe("idf graph", () => {
  it("renders a card for every region plus the synthetic root", () => {
    const nRegions = choose.steps.reduce((n, s) => n + s.regions.length, 0);
    const cards = render(choose).querySelectorAll(".imdx-idf-node");
    expect(cards.length).toBe(nRegions + 1); // + root
  });

  it("draws one edge per parent->child link", () => {
    expect(render(choose).querySelectorAll(".imdx-idf-edge").length).toBe(
      choose.edges.length,
    );
  });

  it("renders one step-axis label per step, plus the root label", () => {
    const labels = render(choose).querySelectorAll(".imdx-idf-axislabel");
    expect(labels.length).toBe(choose.steps.length + 1);
  });

  it("draws one step-hull outline per step, each with a path", () => {
    const hulls = render(choose).querySelectorAll(".imdx-idf-hull");
    expect(hulls.length).toBe(choose.steps.length); // root has no hull
    for (const h of hulls) expect(h.getAttribute("d")).toBeTruthy();
  });

  it("shows the region body on a node click", () => {
    const el = render(choose);
    tap(byNodeId(el, 7)); // a leaf in step 2
    const detail = el.querySelector(".imdx-idf-detail").textContent;
    expect(detail).toContain("#7");
    expect(detail).toContain("Constraints");
    expect(detail).toContain("Invariant");
  });

  it("shows step metadata when a step-axis label is clicked", () => {
    const el = render(choose);
    const labels = el.querySelectorAll(".imdx-idf-axislabel");
    // labels[0] is the root row; labels[1] is Step 0.
    click(labels[1]);
    const detail = el.querySelector(".imdx-idf-detail").textContent;
    expect(detail).toContain("Step 0");
    expect(detail).toContain("Guard");
    expect(detail).toContain("Target");
  });

  it("highlights a selected node's card", () => {
    const el = render(choose);
    const card = byNodeId(el, 7);
    tap(card);
    expect(card.classList.contains("is-selected")).toBe(true);
  });

  it("drags a node, moving its card and rerouting an incident edge", () => {
    const el = render(choose);
    const card = byNodeId(el, 1); // a step-0 node with children (edges incident)
    const edge = el.querySelector(".imdx-idf-edge"); // root -> 1
    const beforeLeft = card.style.left;
    const beforeD = edge.getAttribute("d");
    card.dispatchEvent(new MouseEvent("pointerdown", { bubbles: true, clientX: 0, clientY: 0 }));
    card.dispatchEvent(new MouseEvent("pointermove", { bubbles: true, clientX: 90, clientY: 90 }));
    card.dispatchEvent(new MouseEvent("pointerup", { bubbles: true }));
    expect(card.style.left).not.toBe(beforeLeft);
    expect(edge.getAttribute("d")).not.toBe(beforeD);
    // a real drag is not treated as a selecting click
    expect(card.classList.contains("is-selected")).toBe(false);
  });

  it("selects a step when its hull is clicked", () => {
    const el = render(choose);
    const hull = el.querySelector(".imdx-idf-hull"); // step 0's hull (lowest step)
    hull.dispatchEvent(new MouseEvent("pointerdown", { bubbles: true, clientX: 0, clientY: 0 }));
    hull.dispatchEvent(new MouseEvent("pointerup", { bubbles: true, clientX: 0, clientY: 0 }));
    const detail = el.querySelector(".imdx-idf-detail").textContent;
    expect(detail).toContain("Step");
    expect(detail).toContain("Guard");
  });

  it("offers a reset-view button", () => {
    expect(render(choose).querySelector(".imdx-idf-reset")).not.toBeNull();
  });

  it("tolerates an empty view (decomposition error)", () => {
    const el = render({ steps: [], edges: [] });
    expect(el.querySelectorAll(".imdx-idf-node").length).toBe(0);
    expect(el.querySelector(".imdx-idf-placeholder")).not.toBeNull();
  });
});
