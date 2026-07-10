// Browser entry for the interactive gallery. Bundled to an inlined IIFE by
// build_gallery.mjs, so the resulting HTML runs the widgets live (clicks,
// hovers, zoom) when opened directly from the filesystem — no server needed.

import { drawGraph } from "../../src/idf/graph";
import { drawTreemap } from "../../src/region_decomp/treemap";
import { drawTasks } from "../../src/task/view";

// Fixtures are read from disk and inlined at build time by build_gallery.mjs
// (the browser has no filesystem access). The build replaces __GALLERY_EXAMPLES__
// with a `[{ name, type, data }]` array, where each `data` is the exact widget
// input the Python side syncs to the frontend — the views consume it as-is.
const examples = __GALLERY_EXAMPLES__;

// One tab per widget type; `type` matches the fixture filename prefix.
const TABS = [
  { type: "decomp", label: "Region Decomposition", draw: drawTreemap },
  { type: "idf", label: "IDF", draw: drawGraph },
  { type: "tasks", label: "Tasks", draw: drawTasks },
];

const wrap = document.querySelector(".wrap");

const tabBar = document.createElement("div");
tabBar.className = "tabs";
wrap.appendChild(tabBar);

const buttons = {};
const panels = {};
const built = {};

// Render a panel's widgets on first reveal. Deferring until the panel is visible
// lets the treemap measure a real width instead of falling back to its default.
function buildPanel({ type, draw }) {
  if (built[type]) return;
  built[type] = true;
  for (const ex of examples) {
    if (ex.type !== type) continue;
    const section = document.createElement("section");
    const h2 = document.createElement("h2");
    h2.textContent = ex.name;
    const mount = document.createElement("div");
    section.append(h2, mount);
    panels[type].appendChild(section);
    draw(mount, ex.data);
  }
}

function selectTab(active) {
  for (const tab of TABS) {
    const on = tab.type === active.type;
    buttons[tab.type].classList.toggle("-active", on);
    panels[tab.type].hidden = !on;
  }
  buildPanel(active);
}

for (const tab of TABS) {
  const btn = document.createElement("button");
  btn.type = "button";
  btn.className = "tab";
  btn.textContent = tab.label;
  btn.addEventListener("click", () => selectTab(tab));
  tabBar.appendChild(btn);
  buttons[tab.type] = btn;

  const panel = document.createElement("div");
  panel.className = "panel";
  panel.hidden = true;
  wrap.appendChild(panel);
  panels[tab.type] = panel;
}

selectTab(TABS[0]);
