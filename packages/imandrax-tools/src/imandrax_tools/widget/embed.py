"""
Standalone (comm-less) HTML rendering of anywidgets.

Emit a fully self-contained page: the widget's `_esm`/`_css` are
inlined and driven by a minimal mock model implementing just the surface the
bundles use (`get`/`set`/`on`/`off`/`save_changes`).
"""

from __future__ import annotations

import json
import uuid
from pathlib import Path
from typing import Any

import anywidget


def _synced_state(w: anywidget.AnyWidget) -> dict[str, Any]:
    """User-facing synced traits (excludes internal `_*` and `layout`)."""
    return {
        name: getattr(w, name)
        for name in w.traits(sync=True)
        if not name.startswith('_') and name != 'layout'
    }


def _anywidget_asset(source: Any) -> str:
    """
    Read an anywidget `_esm`/`_css` value (path, FileContents, or str).

    On a widget instance these are already resolved to their string contents;
    `Path`/`FileContents` handling covers class-level access.
    """
    if source is None:
        return ''
    if isinstance(source, Path):
        return source.read_text()
    # Raw strings and `FileContents` both stringify to their contents.
    return str(source)


_STANDALONE_TEMPLATE = """<div id="{el_id}"></div>
<script type="module">
const state = {state_json};
const esmCode = {esm_json};
const cssCode = {css_json};

const el = document.getElementById({el_id_json});

if (cssCode) {{
  const style = document.createElement("style");
  style.textContent = cssCode;
  document.head.appendChild(style);
}}

const listeners = {{}};
const model = {{
  _state: state,
  get(key) {{ return this._state[key]; }},
  set(key, value) {{
    this._state[key] = value;
    (listeners["change:" + key] || []).forEach((cb) => cb());
    (listeners["change"] || []).forEach((cb) => cb());
  }},
  on(event, cb) {{ (listeners[event] ||= []).push(cb); }},
  off(event, cb) {{
    if (!listeners[event]) return;
    listeners[event] = cb
      ? listeners[event].filter((f) => f !== cb)
      : [];
  }},
  save_changes() {{}},
}};

const url = URL.createObjectURL(
  new Blob([esmCode], {{ type: "text/javascript" }})
);
try {{
  const mod = await import(url);
  let def = mod.default;
  if (typeof def === "function") def = await def();
  if (def && typeof def.initialize === "function") {{
    await def.initialize({{ model }});
  }}
  await def.render({{ model, el }});
}} finally {{
  URL.revokeObjectURL(url);
}}
</script>"""


_FULL_PAGE_TEMPLATE = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>{title}</title>
</head>
<body>
{snippet}
</body>
</html>"""


def render_anywidget(
    w: anywidget.AnyWidget, title: str, full_page: bool = False
) -> str:
    """
    Render an anywidget to self-contained HTML with no CDN dependency.

    Args:
        w: The anywidget instance to render.
        title: Document `<title>` used when `full_page` is set.
        full_page: Wrap the widget snippet in a complete HTML document.

    Returns:
        An HTML string: a bare `<div>` + `<script>` snippet, or a full page.

    """
    esm = _anywidget_asset(getattr(w, '_esm', None))
    css = _anywidget_asset(getattr(w, '_css', None))
    el_id = f'widget-{uuid.uuid4().hex}'

    snippet = _STANDALONE_TEMPLATE.format(
        el_id=el_id,
        el_id_json=json.dumps(el_id),
        state_json=json.dumps(_synced_state(w)),
        esm_json=json.dumps(esm),
        css_json=json.dumps(css),
    )

    if full_page:
        return _FULL_PAGE_TEMPLATE.format(title=title, snippet=snippet)
    return snippet
