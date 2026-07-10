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

  it("renders one clickable step band per step, plus the root band", () => {
    const bands = render(choose).querySelectorAll(".imdx-idf-band");
    expect(bands.length).toBe(choose.steps.length + 1);
  });

  it("shows the region body on a node click", () => {
    const el = render(choose);
    click(byNodeId(el, 7)); // a leaf in step 2
    const detail = el.querySelector(".imdx-idf-detail").textContent;
    expect(detail).toContain("#7");
    expect(detail).toContain("Constraint path");
    expect(detail).toContain("Invariant");
  });

  it("shows step metadata when a step band is clicked", () => {
    const el = render(choose);
    const bands = el.querySelectorAll(".imdx-idf-band");
    // bands[0] is the root row; bands[1] is Step 0.
    click(bands[1]);
    const detail = el.querySelector(".imdx-idf-detail").textContent;
    expect(detail).toContain("Step 0");
    expect(detail).toContain("Guard");
    expect(detail).toContain("Target");
  });

  it("highlights a selected node's card", () => {
    const el = render(choose);
    const card = byNodeId(el, 7);
    click(card);
    expect(card.classList.contains("is-selected")).toBe(true);
  });

  it("tolerates an empty view (decomposition error)", () => {
    const el = render({ steps: [], edges: [] });
    expect(el.querySelectorAll(".imdx-idf-node").length).toBe(0);
    expect(el.querySelector(".imdx-idf-placeholder")).not.toBeNull();
  });
});
