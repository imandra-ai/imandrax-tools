var p=new RegExp([/(?<str>'''[\s\S]*?'''|"""[\s\S]*?"""|'(?:[^'\\]|\\.)*'|"(?:[^"\\]|\\.)*")/,/(?<lit>\b(?:None|True|False)\b)/,/(?<cls>[A-Za-z_]\w*(?=\())/,/(?<attr>[A-Za-z_]\w*(?=\s*=))/,/(?<ident>[A-Za-z_]\w*)/,/(?<num>-?\d+(?:\.\d+)?)/].map(t=>t.source).join("|"),"g"),m={str:"t-str",lit:"t-lit",cls:"t-cls",attr:"t-attr",num:"t-num"};function d(t){return t.replace(/[&<>]/g,n=>n==="&"?"&amp;":n==="<"?"&lt;":"&gt;")}function u(t){let n="",o=0;for(let a=p.exec(t);a;a=p.exec(t)){n+=d(t.slice(o,a.index));let s=a.groups??{},r=Object.keys(m).find(i=>s[i]!==void 0);n+=r?`<span class="${m[r]}">${d(a[0])}</span>`:d(a[0]),o=a.index+a[0].length}return n+=d(t.slice(o)),n}var e="imdx-task",f=`
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
`;function x(t){let n=document.createElement("details");n.className=`${e}-art`,n.open=!0;let o=document.createElement("summary");o.className=`${e}-summary`;let a=document.createElement("span");a.className=`${e}-art-kind`,a.textContent=t.kind,o.appendChild(a);let s=document.createElement("span");if(s.className=`${e}-meta`,s.textContent=`${t.text.length.toLocaleString()} chars`,o.appendChild(s),t.icon){let l=document.createElement("span");l.className=`${e}-art-icon`,l.textContent=t.icon,o.appendChild(l)}let r=document.createElement("button");r.className=`${e}-copy`,r.type="button",r.textContent="copy",r.addEventListener("click",l=>{l.preventDefault(),l.stopPropagation(),navigator.clipboard?.writeText(t.text).then(()=>{r.textContent="copied",setTimeout(()=>r.textContent="copy",1200)})}),o.appendChild(r),n.appendChild(o);let i=document.createElement("div");i.className=`${e}-scroll`;let c=document.createElement("pre");return c.className=`${e}-pre`,c.innerHTML=u(t.text),i.appendChild(c),n.appendChild(i),n}function h(t){let n=document.createElement("details");n.className=`${e}-task`,n.open=!0;let o=document.createElement("summary");o.className=`${e}-summary`;let a=document.createElement("span");if(a.className=`${e}-kind`,a.textContent=t.kind,o.appendChild(a),t.id){let c=document.createElement("span");c.className=`${e}-id`,c.textContent=t.id,o.appendChild(c)}let s=document.createElement("span");s.className=`${e}-meta`;let r=t.artifacts.length;s.textContent=`${r} artifact${r===1?"":"s"}`,o.appendChild(s),n.appendChild(o);let i=document.createElement("div");i.className=`${e}-body`;for(let c of t.artifacts)i.appendChild(x(c));return n.appendChild(i),n}function g(t,n){t.innerHTML="",t.classList.add(e);let o=document.createElement("style");if(o.textContent=f,t.appendChild(o),!n||n.length===0){let a=document.createElement("div");a.className=`${e}-placeholder`,a.textContent="No tasks.",t.appendChild(a);return}for(let a of n)t.appendChild(h(a))}var T={render({model:t,el:n}){let o=()=>g(n,t.get("task_entries"));o(),t.on("change:task_entries",o)}};export{T as default};
