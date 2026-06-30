// Browser entry for the interactive gallery. Bundled to an inlined IIFE by
// build_gallery.mjs, so the resulting HTML runs `draw` live (clicks and
// hovers work) when opened directly from the filesystem — no server needed.

import { draw } from "../../src/region_decomp/draw";

// Fixtures are read from disk and inlined at build time by
// build_gallery.mjs (the browser has no filesystem access). The build
// replaces __GALLERY_EXAMPLES__ with the parsed { name: data } object, where
// each `data` is a raw decomposition result — draw consumes it as-is.
const examples = __GALLERY_EXAMPLES__;

const wrap = document.querySelector(".wrap");
for (const [name, data] of Object.entries(examples)) {
  const section = document.createElement("section");
  const h2 = document.createElement("h2");
  h2.textContent = name;
  const mount = document.createElement("div");
  section.append(h2, mount);
  wrap.appendChild(section);
  draw(mount, data);
}
