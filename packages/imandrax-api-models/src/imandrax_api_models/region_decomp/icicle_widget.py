def icicle_widget_html(widget_id: str, data_json: str) -> str:
    return f"""\
<div id="{widget_id}" style="display:flex; font-family:monospace; font-size:13px; border:1px solid #ccc; border-radius:4px; overflow:hidden; height:500px;">
  <div id="{widget_id}-chart" style="flex:3; min-width:0; overflow:auto;"></div>
  <div style="width:1px; background:#ccc; flex-shrink:0;"></div>
  <div id="{widget_id}-detail" style="flex:2; min-width:0; padding:12px; overflow-y:auto; background:#fafafa;">
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

  // Icicle chart
  // ------------
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

  // Labels: label_path (line 1), introduced_constraint (line 2), invariant (line 3)
  const fontSize = "10px";
  const lineH = 12;
  cell.each(function(d) {{
    const w = d.x1 - d.x0;
    const h = d.y1 - d.y0;
    if (w < 30 || h < lineH) return;

    const g = d3.select(this);
    const clipId = `url(#${{g.select("clipPath").attr("id")}})`;
    let y = lineH;

    // Label: label_path (line 1)
    g.append("text")
      .attr("clip-path", clipId)
      .attr("x", 3).attr("y", y)
      .attr("fill", "#000")
      .style("font-size", fontSize)
      .text("[" + (d.data.label_path || "") + "]");
    y += lineH;

    // Label: introduced_constraint (line 2)
    if (h >= y && d.data.introduced_constraint) {{
      g.append("text")
        .attr("clip-path", clipId)
        .attr("x", 3).attr("y", y)
        .attr("fill", "#000")
        .style("font-size", fontSize)
        .text("cst: " + d.data.introduced_constraint);
      y += lineH;
    }}

    // Label: invariant (line 3)
    if (h >= y && d.data.invariant) {{
      g.append("text")
        .attr("clip-path", clipId)
        .attr("x", 3).attr("y", y)
        .attr("fill", "#333")
        .style("font-size", fontSize)
        .text("inv: " + d.data.invariant);
    }}
  }});

  // Detail panel
  // ------------
  function esc(s) {{
    if (s == null) return "<em>—</em>";
    const el = document.createElement("span");
    el.textContent = String(s);
    return el.innerHTML;
  }}

  function showDetail(d) {{
    let html = `<h3 style="margin:0 0 8px">[${{esc(d.label_path)}}]</h3>`;
    html += `<b>Introduced constraint:</b><br><code>${{esc(d.introduced_constraint)}}</code><br><br>`;
    html += `<b>All constraints:</b><ol style="margin:4px 0;padding-left:20px">`;
    (d.constraints || []).forEach(c => {{ html += `<li><code>${{esc(c)}}</code></li>`; }});
    html += `</ol>`;
    html += `<b>Weight:</b> ${{d.weight}}<br>`;
    html += `<b>Children:</b> ${{d.n_children_regions}}<br>`;
    html += `<b>Descendants:</b> ${{d.n_descendant_regions}}<br><br>`;
    if (d.invariant != null) {{
      html += `<b>Invariant:</b><pre style="white-space:pre-wrap;background:#eee;padding:6px;border-radius:3px">${{esc(d.invariant)}}</pre>`;
    }}
    if (d.example_input != null) {{
      const val = typeof d.example_input === "object"
        ? JSON.stringify(d.example_input, null, 2)
        : String(d.example_input);
      html += `<b>Model:</b><pre style="white-space:pre-wrap;background:#eee;padding:6px;border-radius:3px">${{esc(val)}}</pre>`;
    }}
    if (d.example_output != null) {{
      html += `<b>Model eval:</b><pre style="white-space:pre-wrap;background:#eee;padding:6px;border-radius:3px">${{esc(d.example_output)}}</pre>`;
    }}
    detailEl.innerHTML = html;
  }}
}})();
</script>"""
