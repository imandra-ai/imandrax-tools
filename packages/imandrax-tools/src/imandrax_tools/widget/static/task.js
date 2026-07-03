var e="imdx-task",l=`
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
.${e}-art-kind { font-weight: 600; color: #1a1d21;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }

.${e}-copy { margin-left: auto; font: inherit; font-size: 11px; color: #6b727b;
  background: transparent; border: 1px solid #d8dde2; border-radius: 4px; padding: 1px 6px;
  cursor: pointer; }
.${e}-copy:hover { color: #1a1d21; border-color: #b7c0c9; }

.${e}-scroll { max-height: 720px; overflow: auto; border-top: 1px solid #d8dde2; }
.${e}-pre { margin: 0; padding: 10px; white-space: pre; tab-size: 2; font-size: 12px;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }

.${e}-placeholder { color: #9aa1a9; font-style: italic; padding: 8px; }
`;function m(t){let n=document.createElement("details");n.className=`${e}-art`,n.open=!0;let a=document.createElement("summary");a.className=`${e}-summary`;let o=document.createElement("span");o.className=`${e}-art-kind`,o.textContent=t.kind,a.appendChild(o);let d=document.createElement("span");d.className=`${e}-meta`,d.textContent=`${t.text.length.toLocaleString()} chars`,a.appendChild(d);let r=document.createElement("button");r.className=`${e}-copy`,r.type="button",r.textContent="copy",r.addEventListener("click",c=>{c.preventDefault(),c.stopPropagation(),navigator.clipboard?.writeText(t.text).then(()=>{r.textContent="copied",setTimeout(()=>r.textContent="copy",1200)})}),a.appendChild(r),n.appendChild(a);let i=document.createElement("div");i.className=`${e}-scroll`;let s=document.createElement("pre");return s.className=`${e}-pre`,s.textContent=t.text,i.appendChild(s),n.appendChild(i),n}function f(t){let n=document.createElement("details");n.className=`${e}-task`,n.open=!0;let a=document.createElement("summary");a.className=`${e}-summary`;let o=document.createElement("span");if(o.className=`${e}-kind`,o.textContent=t.kind,a.appendChild(o),t.id){let s=document.createElement("span");s.className=`${e}-id`,s.textContent=t.id,a.appendChild(s)}let d=document.createElement("span");d.className=`${e}-meta`;let r=t.artifacts.length;d.textContent=`${r} artifact${r===1?"":"s"}`,a.appendChild(d),n.appendChild(a);let i=document.createElement("div");i.className=`${e}-body`;for(let s of t.artifacts)i.appendChild(m(s));return n.appendChild(i),n}function p(t,n){t.innerHTML="",t.classList.add(e);let a=document.createElement("style");if(a.textContent=l,t.appendChild(a),!n||n.length===0){let o=document.createElement("div");o.className=`${e}-placeholder`,o.textContent="No tasks.",t.appendChild(o);return}for(let o of n)t.appendChild(f(o))}var b={render({model:t,el:n}){let a=()=>p(n,t.get("task_entries"));a(),t.on("change:task_entries",a)}};export{b as default};
