var O=/^\s*$/,$e=/^(?:-(?:\s+|$))+/,ke=/:[ \t]*(?:#.*)?$/,ye=/(?:^[ \t]*|[:-][ \t]+)[|>][+-]?\d{0,2}[ \t]*(?:#.*)?$/;function Y(e){return/^ */.exec(e)[0].length}function I(e,t){return $e.exec(e.slice(t))?.[0].length??0}function Ee(e,t){return t+Math.max(1,I(e,t))}function Le(e,t){return I(e,t)===0&&ke.test(e)?t:1/0}function ve(e){return ye.test(e)}function Q(e){return{text:e,indent:Y(e),children:[],block:[]}}function ee(e){let t=e.replace(/\n+$/,"").split(`
`),r=[],n=[],l=(i,a,c)=>{let x=n.length?n[n.length-1].node:null;(x?x.children:r).push(i),n.push({node:i,childIndent:a,seqIndent:c})};for(let i=0;i<t.length;i++){let a=t[i];if(O.test(a)){let d=Q(a);n.length?n[n.length-1].node.children.push(d):r.push(d);continue}let c=Y(a),x=I(a,c)>0;for(;n.length;){let d=n[n.length-1],L=x?Math.min(d.childIndent,d.seqIndent):d.childIndent;if(c>=L)break;n.pop()}let h=Q(a);if(l(h,Ee(a,c),Le(a,c)),!!ve(a)){for(;i+1<t.length;){let d=t[i+1];if(!O.test(d)&&Y(d)<=c)break;h.block.push(d),i++}for(;h.block.length&&O.test(h.block[h.block.length-1]);)h.block.pop(),i--;n.pop()}}return r}function K(e){let t=e.block.length;for(let r of e.children)t+=1+K(r);return t}var we=/^(?:-(?:[ \t]+|$))+/,Ce=/^("(?:[^"\\]|\\.)*"|'(?:[^']|'')*'|[^:#\s][^:]*?)(:)([ \t]|$)/,Te=/^[|>][+-]?\d{0,2}$/,Se=/^([&*]\S+|!!?\S*)([ \t]+|$)/,Me=/^-?(?:\d[\d_]*(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$|^-?0[xXoObB][0-9a-fA-F_]+$|^[-+]?\.(?:inf|Inf|INF)$|^\.(?:nan|NaN|NAN)$/,Ne=/^(?:true|True|TRUE|false|False|FALSE|null|Null|NULL|~)$/,He=/^"(?:[^"\\]|\\.)*"|^'(?:[^']|'')*'/;function ne(e){return e.replace(/[&<>]/g,t=>t==="&"?"&amp;":t==="<"?"&lt;":"&gt;")}function C(e,t){return`<span class="t-${e}">${ne(t)}</span>`}function Ae(e){let t=/^[ \t]*/.exec(e)[0].length,r=He.exec(e.slice(t)),n=t+(r?r[0].length:0),l=/(?:^|[ \t])#/.exec(e.slice(n));if(!l)return[e,""];let i=n+l.index;return[e.slice(0,i),e.slice(i)]}function te(e){let[t,r]=Ae(e),n=/^[ \t]*/.exec(t)[0],l=t.slice(n.length),i=n,a=Se.exec(l);if(a&&(i+=C("ref",a[1])+a[2],l=l.slice(a[0].length)),l){let c=Te.test(l)?"block":Ne.test(l)?"lit":Me.test(l)?"num":"str";i+=C(c,l)}return i+(r?C("comment",r):"")}function B(e){let t=/^[ \t]*/.exec(e)[0],r=e.slice(t.length),n=t;if(!r)return n;if(r==="---"||r==="...")return n+C("punct",r);let l=we.exec(r);if(l&&(n+=C("punct",l[0]),r=r.slice(l[0].length)),r.startsWith("#"))return n+C("comment",r);let i=Ce.exec(r);return i?(n+=C("key",i[1])+C("punct",":"),n+te(r.slice(i[1].length+1))):n+te(r)}function oe(e){return ne(e)}var s="imdx-jsonable",re=`
.${s} { font-family: ui-sans-serif, system-ui, sans-serif; font-size: 12px;
  color: #1a1d21; border: 1px solid #d8dde2; border-radius: 6px; overflow: hidden;
  background: #fff; box-sizing: border-box; }
.${s} *, .${s} *::before, .${s} *::after { box-sizing: border-box; }

.${s}-bar { display: flex; align-items: center; gap: 8px; padding: 6px 10px;
  background: #fafbfc; border-bottom: 1px solid #d8dde2; }
.${s}-label { font-weight: 600; letter-spacing: 0.02em; }
.${s}-meta { color: #6b727b; font-size: 11px; font-variant-numeric: tabular-nums; }
.${s}-actions { margin-left: auto; display: flex; gap: 6px; }
.${s}-btn { font: inherit; font-size: 11px; color: #6b727b; background: transparent;
  border: 1px solid #d8dde2; border-radius: 4px; padding: 1px 6px; cursor: pointer; }
.${s}-btn:hover { color: #1a1d21; border-color: #b7c0c9; }

.${s}-scroll { max-height: 720px; overflow: auto; padding: 8px 0; }
.${s}-doc { font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 12px; line-height: 1.5; tab-size: 2; }

.${s}-line { display: flex; align-items: baseline; padding: 0 10px 0 4px; }
.${s}-line:hover { background: #f4f6f8; }
summary.${s}-line { cursor: pointer; user-select: none; list-style: none; }
summary.${s}-line::-webkit-details-marker { display: none; }

/* The fold gutter: same width on foldable and leaf lines, so text stays aligned. */
.${s}-arrow { flex: 0 0 1.1em; color: #9aa1a9; font-size: 9px; line-height: 1.7;
  text-align: center; }
summary.${s}-line > .${s}-arrow::before { content: "\\25B8"; display: inline-block;
  transition: transform 0.12s ease; }
details[open] > summary.${s}-line > .${s}-arrow::before { transform: rotate(90deg); }
summary.${s}-line:hover > .${s}-arrow { color: #1a1d21; }

.${s}-text { white-space: pre; }
.${s}-count { margin-left: 10px; color: #9aa1a9; font-size: 11px; font-style: italic;
  font-variant-numeric: tabular-nums; }
details[open] > summary > .${s}-count { display: none; }

/* Block-scalar bodies (\`key: |\`) \u2014 opaque text, dimmed and rendered verbatim. */
.${s}-block { margin: 0; padding: 0 10px 0 calc(1.1em + 4px); white-space: pre;
  color: #3c4249; }

/* Token colors (see jsonable/highlight.ts); light palette tuned for the #fff bg. */
.${s}-text .t-key { color: #0550ae; }      /* mapping keys */
.${s}-text .t-str { color: #0a7d33; }      /* quoted and plain scalars */
.${s}-text .t-num { color: #953800; }      /* numbers */
.${s}-text .t-lit { color: #cf222e; }      /* true / false / null / ~ */
.${s}-text .t-punct { color: #6b727b; }    /* \`-\`, \`:\`, \`---\` */
.${s}-text .t-ref { color: #8250df; }      /* anchors / aliases / tags */
.${s}-text .t-block { color: #8250df; }    /* \`|\` / \`>\` indicators */
.${s}-text .t-comment { color: #9aa1a9; font-style: italic; }

.${s}-placeholder { color: #9aa1a9; font-style: italic; padding: 10px; }
`;var _e=3;function se(){let e=document.createElement("span");return e.className=`${s}-arrow`,e}function ie(e){let t=document.createElement("span");return t.className=`${s}-text`,t.innerHTML=e,t}function ze(e){let t=document.createElement("div");return t.className=`${s}-block`,t.innerHTML=e.map(oe).join(`
`),t}function ae(e,t){if(!(e.children.length>0||e.block.length>0)){let c=document.createElement("div");return c.className=`${s}-line`,c.append(se(),ie(B(e.text))),c}let n=document.createElement("details");n.className=`${s}-fold`,n.open=t<_e;let l=document.createElement("summary");l.className=`${s}-line`,l.append(se(),ie(B(e.text)));let i=document.createElement("span");i.className=`${s}-count`;let a=K(e);i.textContent=`\u2026${a} line${a===1?"":"s"}`,l.appendChild(i),n.appendChild(l),e.block.length&&n.appendChild(ze(e.block));for(let c of e.children)n.appendChild(ae(c,t+1));return n}function D(e,t,r=""){e.innerHTML="",e.classList.add(s);let n=document.createElement("style");if(n.textContent=re,e.appendChild(n),!t||!t.trim()){let c=document.createElement("div");c.className=`${s}-placeholder`,c.textContent="Nothing to show.",e.appendChild(c);return}let l=ee(t),i=document.createElement("div");i.className=`${s}-doc`;for(let c of l)i.appendChild(ae(c,0));let a=document.createElement("div");a.className=`${s}-scroll`,a.appendChild(i),e.appendChild(Re(i,t,r)),e.appendChild(a)}function Re(e,t,r){let n=document.createElement("div");if(n.className=`${s}-bar`,r){let d=document.createElement("span");d.className=`${s}-label`,d.textContent=r,n.appendChild(d)}let l=document.createElement("span");l.className=`${s}-meta`;let i=t.replace(/\n+$/,"").split(`
`).length;l.textContent=`${i.toLocaleString()} line${i===1?"":"s"}`,n.appendChild(l);let a=document.createElement("div");a.className=`${s}-actions`;let c=(d,L)=>{let E=document.createElement("button");return E.className=`${s}-btn`,E.type="button",E.textContent=d,E.addEventListener("click",L),a.appendChild(E),E},x=d=>{for(let L of e.querySelectorAll("details"))L.open=d};c("expand all",()=>x(!0)),c("collapse all",()=>x(!1));let h=c("copy",()=>{navigator.clipboard?.writeText(t).then(()=>{h.textContent="copied",setTimeout(()=>h.textContent="copy",1200)})});return n.appendChild(a),n}var _="imdx-stack",Oe=`
.${_} { display: flex; flex-direction: column; gap: 8px; box-sizing: border-box; }
.${_}-placeholder { font-family: ui-sans-serif, system-ui, sans-serif;
  font-size: 12px; color: #9aa1a9; font-style: italic; padding: 10px;
  border: 1px solid #d8dde2; border-radius: 6px; background: #fff; }
`;function le(e,t){e.innerHTML="",e.classList.add(_);let r=document.createElement("style");r.textContent=Oe,e.appendChild(r);let n=()=>{let a=document.createElement("div");return e.appendChild(a),a},l=!!(t.pre&&t.pre.trim()),i=!!(t.post&&t.post.trim());if(l&&D(n(),t.pre),t.hasMain&&t.main(n()),i&&D(n(),t.post),!l&&!i&&!t.hasMain){let a=document.createElement("div");a.className=`${_}-placeholder`,a.textContent="Nothing to show.",e.appendChild(a)}}var de=new RegExp([/(?<str>'''[\s\S]*?'''|"""[\s\S]*?"""|'(?:[^'\\]|\\.)*'|"(?:[^"\\]|\\.)*")/,/(?<lit>\b(?:None|True|False)\b)/,/(?<cls>[A-Za-z_]\w*(?=\())/,/(?<attr>[A-Za-z_]\w*(?=\s*=))/,/(?<ident>[A-Za-z_]\w*)/,/(?<num>-?\d+(?:\.\d+)?)/].map(e=>e.source).join("|"),"g"),ce={str:"t-str",lit:"t-lit",cls:"t-cls",attr:"t-attr",num:"t-num"};function z(e){return e.replace(/[&<>]/g,t=>t==="&"?"&amp;":t==="<"?"&lt;":"&gt;")}function pe(e){let t="",r=0;for(let n=de.exec(e);n;n=de.exec(e)){t+=z(e.slice(r,n.index));let l=n.groups??{},i=Object.keys(ce).find(a=>l[a]!==void 0);t+=i?`<span class="${ce[i]}">${z(n[0])}</span>`:z(n[0]),r=n.index+n[0].length}return t+=z(e.slice(r)),t}var o="imdx-task",fe=`
.${o} { display: flex; flex-direction: column; gap: 8px;
  font-family: ui-sans-serif, system-ui, sans-serif; font-size: 12px;
  color: #1a1d21; box-sizing: border-box; }
.${o} *, .${o} *::before, .${o} *::after { box-sizing: border-box; }

.${o}-table { width: 100%; border-collapse: collapse; border: 1px solid #d8dde2;
  border-radius: 6px; overflow: hidden; background: #fafbfc; }
.${o}-table th { text-align: left; font-weight: 600; color: #6b727b; font-size: 11px;
  padding: 5px 10px; border-bottom: 1px solid #d8dde2; background: #fff; }
.${o}-table td { padding: 4px 10px; vertical-align: middle; }
.${o}-row + .${o}-row > td,
.${o}-detail + .${o}-row > td { border-top: 1px solid #eef1f4; }
.${o}-row[data-level="error"] { background: #fff5f5; }
.${o}-row[data-level="warning"] { background: #fffaeb; }
.${o}-descr { display: flex; align-items: center; gap: 6px; }
.${o}-descr-none { color: #9aa1a9; }
.${o}-res-descr { white-space: nowrap; }
.${o}-level { margin-left: auto; }
.${o}-row-toggle { cursor: pointer; }
/* Same shade as an artifact title on hover; level tints darken a step. */
.${o}-row-toggle:hover { background: #f6f8fa; }
.${o}-row-toggle[data-level="error"]:hover { background: #ffecec; }
.${o}-row-toggle[data-level="warning"]:hover { background: #fff3d6; }
.${o}-row-toggle:focus-visible { outline: 2px solid #b7c0c9; outline-offset: -2px; }
.${o}-sym-none { color: #9aa1a9; }
.${o}-kind { color: #6b727b; font-size: 11px; letter-spacing: 0.02em; }
.${o}-id { color: #9aa1a9; font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 11px; white-space: nowrap; }
.${o}-id-head { display: flex; align-items: center; gap: 10px; }
.${o}-show-debug { margin-left: auto; display: inline-flex; align-items: center; gap: 4px;
  font-weight: 400; white-space: nowrap; cursor: pointer; user-select: none; }
.${o}-show-debug input { margin: 0; cursor: pointer; }
.${o}-show-debug-none { opacity: 0.5; }
.${o}-meta { color: #6b727b; font-size: 11px; font-variant-numeric: tabular-nums; }

.${o}-chips { display: flex; gap: 4px; flex-wrap: wrap; }
.${o}-chip { font: inherit; font-size: 11px; cursor: pointer; padding: 1px 7px;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  color: #6b727b; background: #fff; border: 1px solid #d8dde2; border-radius: 10px; }
.${o}-chip:hover { color: #1a1d21; border-color: #b7c0c9; }
.${o}-chip[aria-pressed="true"] { color: #1a1d21; background: #e3e8ee; border-color: #b7c0c9; }
/* A folded task's open artifacts: still open, but out of sight. */
.${o}-row-folded .${o}-chip[aria-pressed="true"] { color: #6b727b;
  background: #f1f3f5; border-color: #d8dde2; }

.${o}-detail > td { padding: 0 10px 8px; }
.${o}-detail > td > * + * { margin-top: 6px; }
.${o}-art { border: 1px solid #d8dde2; border-radius: 6px; overflow: hidden;
  background: #fff; }
.${o}-art-head { display: flex; align-items: center; gap: 8px; padding: 4px 10px;
  cursor: pointer; user-select: none; }
.${o}-art-head:hover { background: #f6f8fa; }
.${o}-art-head:focus-visible { outline: 2px solid #b7c0c9; outline-offset: -2px; }
.${o}-art-collapsed .${o}-scroll { display: none; }
.${o}-art-kind { font-weight: 600; color: #1a1d21;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }

.${o}-copy { margin-left: auto; }
.${o}-copy, .${o}-close { font: inherit; font-size: 11px; color: #6b727b;
  background: transparent; border: 1px solid #d8dde2; border-radius: 4px; padding: 1px 6px;
  cursor: pointer; }
.${o}-copy:hover, .${o}-close:hover { color: #1a1d21; border-color: #b7c0c9; }

.${o}-scroll { max-height: 720px; overflow: auto; border-top: 1px solid #d8dde2; }
.${o}-pre { margin: 0; padding: 10px; white-space: pre; tab-size: 2; font-size: 12px;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }

/* Syntax highlighting for the Python-repr artifact text (see task/highlight.ts).
   Light palette tuned for the #fff code bg. */
.${o}-pre .t-cls { color: #8250df; }   /* constructor / class names */
.${o}-pre .t-attr { color: #0550ae; }  /* keyword-arg names */
.${o}-pre .t-str { color: #0a7d33; }   /* string literals */
.${o}-pre .t-num { color: #953800; }   /* numbers */
.${o}-pre .t-lit { color: #cf222e; }   /* None / True / False */

.${o}-placeholder { color: #9aa1a9; font-style: italic; padding: 8px; }
`;var Ye=["debug","info","warning","error"],Ie={error:"\u274C",warning:"\u26A0\uFE0F",info:"\u2705",debug:"\u{1F4A1}"},Ke=4,Be="info";function S(e){return Ye.indexOf(e)}function M(e){return e.level??"info"}function De(e){let[t,r,...n]=e.split(":");return n.length?`${t}:${r}:${n.join(":").slice(0,6)}`:e}function u(e,t,r){let n=document.createElement(e);return t&&(n.className=`${o}-${t}`),r!==void 0&&(n.textContent=r),n}function ue(e,t){let r=u("td",e),n=u("div","descr"),l=u("span","descr-text",t||"\u2014");return t||l.classList.add(`${o}-descr-none`),n.appendChild(l),r.appendChild(n),r}var F=new WeakMap,me=e=>e.getClientRects().length>0;function ge(e){for(let t of e.querySelectorAll(`.${o}-scroll`))me(t)&&F.set(t,{top:t.scrollTop,left:t.scrollLeft})}function he(e){for(let t of e.querySelectorAll(`.${o}-scroll`)){let r=F.get(t);!r||!me(t)||(t.scrollTop=r.top,t.scrollLeft=r.left,F.delete(t))}}function Fe(e,t){let r=u("div","art"),n=u("div","art-head");n.tabIndex=0;let l=d=>{d&&ge(r),r.classList.toggle(`${o}-art-collapsed`,d),d||he(r),n.setAttribute("aria-expanded",String(!d)),n.title=d?"Expand":"Collapse"},i=()=>l(!r.classList.contains(`${o}-art-collapsed`));l(!1),n.addEventListener("click",i),n.addEventListener("keydown",d=>{d.target!==n||d.key!=="Enter"&&d.key!==" "||(d.preventDefault(),i())}),n.appendChild(u("span","art-kind",e.kind)),n.appendChild(u("span","meta",`${e.repr.length.toLocaleString()} chars`));let a=u("button","copy","copy");a.type="button",a.title="Copy",a.addEventListener("click",d=>{d.stopPropagation(),navigator.clipboard?.writeText(e.repr).then(()=>{a.textContent="copied",setTimeout(()=>a.textContent="copy",1200)})}),n.appendChild(a);let c=u("button","close","\xD7");c.type="button",c.title="Remove",c.setAttribute("aria-label",`Remove ${e.kind}`),c.addEventListener("click",d=>{d.stopPropagation(),t()}),n.appendChild(c),r.appendChild(n);let x=u("div","scroll"),h=u("pre","pre");return h.innerHTML=pe(e.repr),x.appendChild(h),r.appendChild(x),r}function be(e,t){e.innerHTML="",e.classList.add(o);let r=document.createElement("style");if(r.textContent=fe,e.appendChild(r),!t||t.length===0){e.appendChild(u("div","placeholder","No tasks."));return}let n=new Map;t.forEach((p,f)=>{let k=p.from_sym??"";n.has(k)||n.set(k,f)});let l=t.map((p,f)=>({t:p,i:f})).sort((p,f)=>S(M(f.t))-S(M(p.t))||n.get(p.t.from_sym??"")-n.get(f.t.from_sym??"")||p.i-f.i),i=new Map;for(let{t:p,i:f}of l){let k=S(M(p))>=S("warning");i.set(f,new Set(k?p.artifacts.map(m=>m.kind):[]))}let a=u("table","table"),c=document.createElement("thead"),x=document.createElement("tr");for(let p of["task","result","symbol","artifacts","kind"])x.appendChild(u("th",void 0,p));let h=u("th",void 0),d=u("div","id-head");d.appendChild(u("span",void 0,"id"));let L=u("label","show-debug"),E=document.createElement("input");E.type="checkbox",E.addEventListener("change",()=>j()),t.some(p=>M(p)==="debug")||(L.classList.add(`${o}-show-debug-none`),L.title="No debug tasks"),L.append(E,"show debug"),d.appendChild(L),h.appendChild(d),x.appendChild(h),c.appendChild(x),a.appendChild(c);let N=document.createElement("tbody");a.appendChild(N),e.appendChild(a);let q=u("div","placeholder","No tasks at info level or above.");e.appendChild(q);let P=new Map;for(let{t:p,i:f}of l)P.set(f,xe(p,i.get(f)));function j(){N.innerHTML="";let p=S(E.checked?"debug":Be),f=l.filter(({t:k})=>S(M(k))>=p);q.hidden=f.length>0;for(let{i:k}of f){let{row:m,detail:y}=P.get(k);N.appendChild(m),i.get(k).size>0&&N.appendChild(y)}}function xe(p,f){let k=M(p),m=u("tr","row");m.dataset.level=k;let y=u("tr","detail"),H=document.createElement("td");H.colSpan=6,y.appendChild(H);let R=new Map,J=new Map,w=!1,A=()=>{for(let[$,v]of J)v.setAttribute("aria-pressed",String(f.has($)));f.size===0&&(w=!1),m.classList.toggle(`${o}-row-folded`,w),p.artifacts.length>0&&m.setAttribute("aria-expanded",String(f.size>0&&!w));let b=null;for(let $ of p.artifacts){let v=R.get($.kind);if(!f.has($.kind)){v?.remove(),R.delete($.kind);continue}v||(v=Fe($,()=>{f.delete($.kind),A()}),R.set($.kind,v));let T=b?b.nextSibling:H.firstChild;T!==v&&H.insertBefore(v,T),b=v}w&&!y.hidden&&ge(y);let g=!w&&y.hidden;y.hidden=w,f.size===0?y.remove():m.parentNode&&m.nextSibling!==y&&m.after(y),g&&he(y)},U=()=>{if(f.size===0)for(let b of p.artifacts)f.add(b.kind);else w=!w;A()};if(p.artifacts.length>0){m.classList.add(`${o}-row-toggle`),m.tabIndex=0,m.title="Show / hide artifacts";let b=null;m.addEventListener("mousedown",g=>b={x:g.clientX,y:g.clientY}),m.addEventListener("click",g=>{let $=b;if(b=null,!(($?Math.hypot(g.clientX-$.x,g.clientY-$.y)>Ke:!1)||g.detail>2)){if(g.detail===1){let T=window.getSelection();T&&!T.isCollapsed&&m.contains(T.anchorNode)&&T.removeAllRanges()}U()}}),m.addEventListener("keydown",g=>{g.target!==m||g.key!=="Enter"&&g.key!==" "||(g.preventDefault(),U())})}m.appendChild(ue("task-descr",p.task_descr));let X=ue("res-descr",p.res_descr),V=u("span","level",Ie[k]);V.title=k,X.firstElementChild.appendChild(V),m.appendChild(X);let Z=u("td","sym",p.from_sym??"\u2014");p.from_sym==null&&Z.classList.add(`${o}-sym-none`),m.appendChild(Z);let G=u("td","chips");for(let b of p.artifacts){let g=u("button","chip",b.kind);g.type="button",g.addEventListener("click",$=>{$.stopPropagation(),f.has(b.kind)?f.delete(b.kind):f.add(b.kind),w=!1,A()}),J.set(b.kind,g),G.appendChild(g)}m.appendChild(G),m.appendChild(u("td","kind",p.kind.replace(/^TASK_/,"")));let W=u("td","id",De(p.id));return W.title=p.id,m.appendChild(W),A(),{row:m,detail:y}}j()}var qe=["task_entries","pre","post"],it={render({model:e,el:t}){let r=()=>{let n=e.get("task_entries");le(t,{pre:e.get("pre"),post:e.get("post"),main:l=>be(l,n??[]),hasMain:n!=null})};r();for(let n of qe)e.on(`change:${n}`,r)}};export{it as default};
