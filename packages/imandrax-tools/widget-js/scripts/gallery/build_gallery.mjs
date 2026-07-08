// Build the interactive gallery: bundle gallery_entry.js to an inlined IIFE and
// wrap it in a self-contained HTML page. Open the output directly in a browser
// (file://) — the widgets run live, so clicks, hovers, and zoom work.
//
//   npm run gallery   ->  dist/widgets.gallery.html

import { build } from "esbuild";
import { mkdirSync, readdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const root = resolve(here, "../..");
const outFile = resolve(root, "dist/widgets.gallery.html");

// Read the widget-input fixtures here (Node has a filesystem; the browser does
// not) and inline them into the bundle via esbuild's `define`. These are the
// same fixtures the tests load — the exact traitlet lists the Python side feeds
// each widget. The filename prefix (`decomp` / `tasks`) picks the widget.
const fixtureDir = resolve(root, "test/fixtures/inputs");

const examples = [];
for (const file of readdirSync(fixtureDir).sort()) {
  if (!file.endsWith(".widget_input.json")) continue;
  // Filenames are `<type>.<name>.[<fn>.]iml.widget_input.json`.
  const [type, name] = file.split("."); // e.g. "decomp","simple" / "tasks","admit_rec"
  const data = JSON.parse(readFileSync(resolve(fixtureDir, file), "utf8"));
  examples.push({ name, type, data });
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
  h1 { font-size: 22px; margin: 0 0 16px; letter-spacing: -0.01em; }
  .tabs { display: flex; gap: 4px; margin: 0 0 24px;
    border-bottom: 1px solid #d8dde2; }
  .tab { appearance: none; border: 0; background: none; font: inherit;
    font-size: 13px; color: #6b727b; cursor: pointer; padding: 8px 14px;
    border-bottom: 2px solid transparent; margin-bottom: -1px; }
  .tab:hover { color: #1a1d21; }
  .tab.-active { color: #1a1d21; border-bottom-color: #1a1d21; font-weight: 600; }
  section { margin-bottom: 32px; }
  h2 { font-size: 14px; margin: 0 0 10px; }
`;

const html = `<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>ImandraX Widgets — gallery</title>
  <style>${PAGE_CSS}</style>
</head>
<body>
  <div class="wrap">
    <h1>ImandraX Widgets</h1>
  </div>
  <script>${bundleJs}</script>
</body>
</html>
`;

mkdirSync(dirname(outFile), { recursive: true });
writeFileSync(outFile, html);
console.log(`wrote ${outFile} (${(html.length / 1024).toFixed(1)} kb)`);
