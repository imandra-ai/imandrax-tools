def icicle_widget_html(widget_id: str, data_json: str) -> str:
    return f"""\
<div id="{widget_id}" style="display:flex; font-family:monospace; font-size:13px; border:1px solid #ccc; border-radius:4px; overflow:hidden;">
  <div id="{widget_id}-chart" style="flex:1; min-width:0;"></div>
  <div id="{widget_id}-detail" style="width:360px; padding:12px; border-left:1px solid #ccc; overflow-y:auto; background:#fafafa;">
    <em>Click a region to see details.</em>
  </div>
</div>
<script src="https://d3js.org/d3.v7.min.js"></script>
<script>
(function() {{
  const widgetId = "{widget_id}";
  const data = {data_json};

  const chartEl = document.getElementById(widgetId + "-chart");
  const detailEl = document.getElementById(widgetId + "-detail");

  const width = chartEl.clientWidth || 600;
  const height = 500;

  const root = d3.hierarchy(data)
    .sum(d => d.children ? 0 : d.weight)
    .sort((a, b) => b.value - a.value);

  d3.partition().size([width, height]).padding(1)(root);

  const color = d3.scaleOrdinal(d3.schemeTableau10);

  const svg = d3.select(chartEl).append("svg")
    .attr("width", width)
    .attr("height", height)
    .attr("viewBox", [0, 0, width, height]);

  const cell = svg.selectAll("g")
    .data(root.descendants())
    .join("g")
    .attr("transform", d => `translate(${{d.x0}},${{d.y0}})`);

  cell.append("rect")
    .attr("width", d => Math.max(0, d.x1 - d.x0))
    .attr("height", d => Math.max(0, d.y1 - d.y0))
    .attr("fill", d => color(d.depth))
    .attr("stroke", "#fff")
    .attr("stroke-width", 0.5)
    .style("cursor", "pointer")
    .on("click", (ev, d) => showDetail(d.data));

  cell.append("clipPath")
    .attr("id", (d, i) => widgetId + "-clip-" + i)
    .append("rect")
    .attr("width", d => Math.max(0, d.x1 - d.x0))
    .attr("height", d => Math.max(0, d.y1 - d.y0));

  cell.append("text")
    .attr("clip-path", (d, i) => `url(#${{widgetId}}-clip-${{i}})`)
    .attr("x", 3)
    .attr("y", 13)
    .attr("fill", "#000")
    .style("font-size", "11px")
    .text(d => {{
      const w = d.x1 - d.x0;
      if (w < 30) return "";
      return d.data.label_path || "";
    }});

  function esc(s) {{
    if (s == null) return "<em>—</em>";
    const el = document.createElement("span");
    el.textContent = String(s);
    return el.innerHTML;
  }}

  function showDetail(d) {{
    let html = `<h3 style="margin:0 0 8px">[${{esc(d.label_path)}}]</h3>`;
    html += `<b>Introduced constraint:</b><br><code>${{esc(d.introduced_constraint)}}</code><br><br>`;
    html += `<b>Full constraint path:</b><ol style="margin:4px 0;padding-left:20px">`;
    (d.constraints || []).forEach(c => {{ html += `<li><code>${{esc(c)}}</code></li>`; }});
    html += `</ol>`;
    html += `<b>Weight:</b> ${{d.weight}}<br>`;
    html += `<b>Children:</b> ${{d.n_children_regions}}<br>`;
    html += `<b>Descendants:</b> ${{d.n_descendant_regions}}<br><br>`;
    if (d.invariant != null) {{
      html += `<b>Invariant:</b><pre style="white-space:pre-wrap;background:#eee;padding:6px;border-radius:3px">${{esc(d.invariant)}}</pre>`;
    }}
    if (d.example_input != null) {{
      html += `<b>Example input:</b><pre style="white-space:pre-wrap;background:#eee;padding:6px;border-radius:3px">${{esc(d.example_input)}}</pre>`;
    }}
    if (d.example_output != null) {{
      html += `<b>Example output:</b><pre style="white-space:pre-wrap;background:#eee;padding:6px;border-radius:3px">${{esc(d.example_output)}}</pre>`;
    }}
    detailEl.innerHTML = html;
  }}
}})();
</script>"""
