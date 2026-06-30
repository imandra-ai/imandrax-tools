// Build the interactive gallery: bundle gallery_entry.js to an inlined IIFE and
// wrap it in a self-contained HTML page. Open the output directly in a browser
// (file://) — the widget runs live, so clicks and hovers work.
//
//   npm run gallery   ->  dist/region_decomp.gallery.html

import { build } from "esbuild";
import { mkdirSync, readdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const root = resolve(here, "../..");
const outFile = resolve(root, "dist/region_decomp.gallery.html");

// Read the .jsonc fixtures here (Node has a filesystem; the browser does not)
// and inline them into the bundle via esbuild's `define`. The fixtures carry a
// leading `//` header line, so strip line comments before parsing.
const fixtureDir = resolve(root, "test/fixtures/region_decomp");
const stripLineComments = (text) => text.replace(/^\s*\/\/.*$/gm, "");

const examples = {};
for (const file of readdirSync(fixtureDir).sort()) {
  if (file.endsWith(".jsonc")) {
    const name = file.split(".")[0];
    const raw = readFileSync(resolve(fixtureDir, file), "utf8");
    examples[name] = JSON.parse(stripLineComments(raw));
  }
}

const result = await build({
  entryPoints: [resolve(here, "gallery_entry.js")],
  bundle: true,
  format: "iife",
  minify: true,
  write: false,
  define: {
    __GALLERY_EXAMPLES__: JSON.stringify(examples),
  },
});
const bundleJs = result.outputFiles[0].text;

const PAGE_CSS = `
  body { margin: 0; background: #eef1f4; color: #1a1d21;
    font-family: ui-sans-serif, system-ui, -apple-system, sans-serif; }
  .wrap { max-width: 1120px; margin: 0 auto; padding: 36px 28px 64px; }
  h1 { font-size: 22px; margin: 0 0 4px; letter-spacing: -0.01em; }
  .intro { color: #6b727b; font-size: 13px; margin: 0 0 28px; max-width: 70ch; }
  section { margin-bottom: 32px; }
  h2 { font-size: 14px; margin: 0 0 2px; }
  .caption { color: #6b727b; font-size: 12.5px; margin: 0 0 10px; }
`;

const html = `<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Region Decomposition Widget — gallery</title>
  <style>${PAGE_CSS}</style>
</head>
<body>
  <div class="wrap">
    <h1>Region Decomposition Widget</h1>
    <p class="intro">The icicle view on real fixtures. Width is the number of leaf
      regions under a node, depth is constraint nesting, color marks the top-level
      branch. Click any region for its constraint path, invariant, and example.</p>
  </div>
  <script>${bundleJs}</script>
</body>
</html>
`;

mkdirSync(dirname(outFile), { recursive: true });
writeFileSync(outFile, html);
console.log(`wrote ${outFile} (${(html.length / 1024).toFixed(1)} kb)`);
