var u=new RegExp([/(?<str>'''[\s\S]*?'''|"""[\s\S]*?"""|'(?:[^'\\]|\\.)*'|"(?:[^"\\]|\\.)*")/,/(?<lit>\b(?:None|True|False)\b)/,/(?<cls>[A-Za-z_]\w*(?=\())/,/(?<attr>[A-Za-z_]\w*(?=\s*=))/,/(?<ident>[A-Za-z_]\w*)/,/(?<num>-?\d+(?:\.\d+)?)/].map(t=>t.source).join("|"),"g"),f={str:"t-str",lit:"t-lit",cls:"t-cls",attr:"t-attr",num:"t-num"};function l(t){return t.replace(/[&<>]/g,n=>n==="&"?"&amp;":n==="<"?"&lt;":"&gt;")}function g(t){let n="",o=0;for(let r=u.exec(t);r;r=u.exec(t)){n+=l(t.slice(o,r.index));let c=r.groups??{},s=Object.keys(f).find(a=>c[a]!==void 0);n+=s?`<span class="${f[s]}">${l(r[0])}</span>`:l(r[0]),o=r.index+r[0].length}return n+=l(t.slice(o)),n}var e="imdx-task",x=`
.${e} { display: flex; flex-direction: column; gap: 8px;
  font-family: ui-sans-serif, system-ui, sans-serif; font-size: 12px;
  color: #1a1d21; box-sizing: border-box; }
.${e} *, .${e} *::before, .${e} *::after { box-sizing: border-box; }

.${e}-task, .${e}-art { border: 1px solid #d8dde2; border-radius: 6px;
  overflow: hidden; }
.${e}-task { background: #fafbfc; }

.${e}-summary { display: flex; align-items: center; gap: 8px; padding: 6px 10px;
  cursor: pointer; user-select: none; list-style: none; }
.${e}-summary::-webkit-details-marker { display: none; }
.${e}-summary::before { content: "\\25B8"; color: #6b727b; font-size: 10px;
  transition: transform 0.12s ease; }
details[open] > .${e}-summary::before { transform: rotate(90deg); }

.${e}-kind { font-weight: 600; letter-spacing: 0.02em; }
.${e}-id { color: #6b727b; font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 11px; }
.${e}-meta { margin-left: auto; color: #6b727b; font-size: 11px;
  font-variant-numeric: tabular-nums; }

.${e}-body { padding: 8px; display: flex; flex-direction: column; gap: 8px; }
.${e}-art { background: #fff; }
.${e}-art-icon { font-size: 12px; line-height: 1; }
.${e}-art-kind { font-weight: 600; color: #1a1d21;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }

.${e}-copy { margin-left: auto; font: inherit; font-size: 11px; color: #6b727b;
  background: transparent; border: 1px solid #d8dde2; border-radius: 4px; padding: 1px 6px;
  cursor: pointer; }
.${e}-copy:hover { color: #1a1d21; border-color: #b7c0c9; }

.${e}-scroll { max-height: 720px; overflow: auto; border-top: 1px solid #d8dde2; }
.${e}-pre { margin: 0; padding: 10px; white-space: pre; tab-size: 2; font-size: 12px;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }

/* Syntax highlighting for the Python-repr artifact text (see task/highlight.ts).
   Light palette tuned for the #fff code bg. */
.${e}-pre .t-cls { color: #8250df; }   /* constructor / class names */
.${e}-pre .t-attr { color: #0550ae; }  /* keyword-arg names */
.${e}-pre .t-str { color: #0a7d33; }   /* string literals */
.${e}-pre .t-num { color: #953800; }   /* numbers */
.${e}-pre .t-lit { color: #cf222e; }   /* None / True / False */

.${e}-placeholder { color: #9aa1a9; font-style: italic; padding: 8px; }
`;var m={success:"\u2705",error:"\u274C",warning:"\u26A0\uFE0F",info:"\u2139\uFE0F",in_progress:"\u{1F6A7}",pending:"\u23F3",running:"\u23F1\uFE0F",skipped:"\u23ED\uFE0F",unknown:"\u2753",healthy:"\u{1F7E2}",degraded:"\u{1F7E1}",down:"\u{1F534}"};function $(t,n){if(t==="po_res")return n.includes("res=POSuccessProof")?m.success:n.includes("res=POErrorProof")?m.warning:m.error}function y(t){let n=document.createElement("details");n.className=`${e}-art`,n.open=!0;let o=document.createElement("summary");o.className=`${e}-summary`;let r=document.createElement("span");r.className=`${e}-art-kind`,r.textContent=t.kind,o.appendChild(r);let c=$(t.kind,t.repr);if(c){let d=document.createElement("span");d.className=`${e}-art-icon`,d.textContent=c,o.appendChild(d)}let s=document.createElement("span");s.className=`${e}-meta`,s.textContent=`${t.repr.length.toLocaleString()} chars`,o.appendChild(s);let a=document.createElement("button");a.className=`${e}-copy`,a.type="button",a.textContent="copy",a.addEventListener("click",d=>{d.preventDefault(),d.stopPropagation(),navigator.clipboard?.writeText(t.repr).then(()=>{a.textContent="copied",setTimeout(()=>a.textContent="copy",1200)})}),o.appendChild(a),n.appendChild(o);let i=document.createElement("div");i.className=`${e}-scroll`;let p=document.createElement("pre");return p.className=`${e}-pre`,p.innerHTML=g(t.repr),i.appendChild(p),n.appendChild(i),n}function b(t){let n=document.createElement("details");n.className=`${e}-task`,n.open=!0;let o=document.createElement("summary");o.className=`${e}-summary`;let r=document.createElement("span");if(r.className=`${e}-kind`,r.textContent=t.kind,o.appendChild(r),t.id){let i=document.createElement("span");i.className=`${e}-id`,i.textContent=t.id,o.appendChild(i)}let c=document.createElement("span");c.className=`${e}-meta`;let s=t.artifacts.length;c.textContent=`${s} artifact${s===1?"":"s"}`,o.appendChild(c),n.appendChild(o);let a=document.createElement("div");a.className=`${e}-body`;for(let i of t.artifacts)a.appendChild(y(i));return n.appendChild(a),n}function h(t,n){t.innerHTML="",t.classList.add(e);let o=document.createElement("style");if(o.textContent=x,t.appendChild(o),!n||n.length===0){let r=document.createElement("div");r.className=`${e}-placeholder`,r.textContent="No tasks.",t.appendChild(r);return}for(let r of n)t.appendChild(b(r))}var _={render({model:t,el:n}){let o=()=>h(n,t.get("task_entries"));o(),t.on("change:task_entries",o)}};export{_ as default};
