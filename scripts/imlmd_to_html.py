#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "imandrax-tools[extended-client,widget]>=20.7.1,<20.8.0",
#     "markdown-it-py[linkify]>=3",
#     "pygments>=2",
# ]
#
# [tool.uv.sources]
# # imandrax-tools = { path = "../packages/imandrax-tools", editable = true }
#
# ///
# pyright: basic
"""
Render a Markdown file to standalone HTML with IML cell widgets.

The cell widgets of every ```iml fence go right after it.

All fences share one ImandraX client, so later blocks see earlier definitions.

Usage
-----
```bash
uv run --script scripts/md_to_html.py doc.md -o doc.html
```

The client comes from `get_imandrax_client()` (reads `IMANDRAX_URL` and
`IMANDRA_UNI_KEY` / `IMANDRAX_API_KEY` from the environment).
"""

from __future__ import annotations

import argparse
from pathlib import Path

from imandrax_api_models.client import ImandraXClient, get_imandrax_client
from imandrax_tools.widget import render_anywidget
from imandrax_tools.widget.cell_repr import cell_widgets
from markdown_it import MarkdownIt
from markdown_it.renderer import RendererHTML
from pygments import highlight
from pygments.formatters import HtmlFormatter
from pygments.lexers import OcamlLexer

PAGE = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>{title}</title>
<style>
  body {{ max-width: 960px; margin: 0 auto; padding: 16px; line-height: 1.5;
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif; }}
  code, pre {{ font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
    font-size: 0.875em; }}
  pre {{ background: #f5f5f5; padding: 8px; overflow-x: auto; line-height: 1.4; }}
  pre code {{ font-size: inherit; }}
  :not(pre) > code {{ background: #f0f0f0; padding: 0.1em 0.35em; border-radius: 4px; }}
{pygments_css}
</style>
</head>
<body>
{body}
</body>
</html>"""


def highlight_code(code: str, lang: str, _attrs: str) -> str:
    """OCaml highlighting for `iml` / `ocaml` fences; '' keeps the default."""
    if lang in ('iml', 'ocaml'):
        return highlight(code, OcamlLexer(), HtmlFormatter(nowrap=True))
    return ''


def md_to_html(c: ImandraXClient, md_src: str, title: str) -> str:
    md = MarkdownIt('gfm-like', {'highlight': highlight_code})

    def fence(self, tokens, idx, options, env) -> str:
        html = RendererHTML.fence(self, tokens, idx, options, env)
        tok = tokens[idx]
        if tok.info.split()[:1] == ['iml']:
            for w in cell_widgets(c, tok.content):
                html += render_anywidget(w, title=title)
        return html

    md.add_render_rule('fence', fence)
    return PAGE.format(
        title=title,
        pygments_css=HtmlFormatter().get_style_defs('pre'),
        body=md.render(md_src),
    )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('input', help='Markdown file')
    parser.add_argument('-o', '--output', help='HTML file (default: <input>.html)')
    args = parser.parse_args()

    src = Path(args.input)
    out = Path(args.output) if args.output else src.with_suffix('.html')
    out.write_text(md_to_html(get_imandrax_client(), src.read_text(), src.stem))
    print(f'wrote {out}')


if __name__ == '__main__':
    main()
