// Browser entry for the interactive gallery. Bundled to an inlined IIFE by
// build_gallery.mjs, so the resulting HTML runs the widgets live (clicks,
// hovers, zoom) when opened directly from the filesystem — no server needed.

import { drawIcicle } from "../../src/region_decomp/icicle";
import { drawTreemap } from "../../src/region_decomp/treemap";

// Fixtures are read from disk and inlined at build time by build_gallery.mjs
// (the browser has no filesystem access). The build replaces __GALLERY_EXAMPLES__
// with the parsed { name: data } object, where each `data` is a raw
// decomposition result — the views consume it as-is.
const examples = __GALLERY_EXAMPLES__;

const VIEWS = [
  ["treemap", drawTreemap],
  ["icicle", drawIcicle],
];

const wrap = document.querySelector(".wrap");
for (const [name, data] of Object.entries(examples)) {
  for (const [viewName, draw] of VIEWS) {
    const section = document.createElement("section");
    const h2 = document.createElement("h2");
    h2.textContent = `${name} — ${viewName}`;
    const mount = document.createElement("div");
    section.append(h2, mount);
    wrap.appendChild(section);
    draw(mount, data);
  }
}
