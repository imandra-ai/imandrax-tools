var K=/^\s*$/,$e=/^(?:-(?:\s+|$))+/,ke=/:[ \t]*(?:#.*)?$/,ye=/(?:^[ \t]*|[:-][ \t]+)[|>][+-]?\d{0,2}[ \t]*(?:#.*)?$/;function B(e){return/^ */.exec(e)[0].length}function D(e,t){return $e.exec(e.slice(t))?.[0].length??0}function Ee(e,t){return t+Math.max(1,D(e,t))}function ve(e,t){return D(e,t)===0&&ke.test(e)?t:1/0}function Le(e){return ye.test(e)}function Q(e){return{text:e,indent:B(e),children:[],block:[]}}function ee(e){let t=e.replace(/\n+$/,"").split(`
`),r=[],n=[],i=(a,l,d)=>{let $=n.length?n[n.length-1].node:null;($?$.children:r).push(a),n.push({node:a,childIndent:l,seqIndent:d})};for(let a=0;a<t.length;a++){let l=t[a];if(K.test(l)){let c=Q(l);n.length?n[n.length-1].node.children.push(c):r.push(c);continue}let d=B(l),$=D(l,d)>0;for(;n.length;){let c=n[n.length-1],L=$?Math.min(c.childIndent,c.seqIndent):c.childIndent;if(d>=L)break;n.pop()}let b=Q(l);if(i(b,Ee(l,d),ve(l,d)),!!Le(l)){for(;a+1<t.length;){let c=t[a+1];if(!K.test(c)&&B(c)<=d)break;b.block.push(c),a++}for(;b.block.length&&K.test(b.block[b.block.length-1]);)b.block.pop(),a--;n.pop()}}return r}function q(e){let t=e.block.length;for(let r of e.children)t+=1+q(r);return t}var we=/^(?:-(?:[ \t]+|$))+/,Ce=/^("(?:[^"\\]|\\.)*"|'(?:[^']|'')*'|[^:#\s][^:]*?)(:)([ \t]|$)/,Te=/^[|>][+-]?\d{0,2}$/,Se=/^([&*]\S+|!!?\S*)([ \t]+|$)/,Me=/^-?(?:\d[\d_]*(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$|^-?0[xXoObB][0-9a-fA-F_]+$|^[-+]?\.(?:inf|Inf|INF)$|^\.(?:nan|NaN|NAN)$/,Ne=/^(?:true|True|TRUE|false|False|FALSE|null|Null|NULL|~)$/,He=/^"(?:[^"\\]|\\.)*"|^'(?:[^']|'')*'/;function ne(e){return e.replace(/[&<>]/g,t=>t==="&"?"&amp;":t==="<"?"&lt;":"&gt;")}function T(e,t){return`<span class="t-${e}">${ne(t)}</span>`}function Ae(e){let t=/^[ \t]*/.exec(e)[0].length,r=He.exec(e.slice(t)),n=t+(r?r[0].length:0),i=/(?:^|[ \t])#/.exec(e.slice(n));if(!i)return[e,""];let a=n+i.index;return[e.slice(0,a),e.slice(a)]}function te(e){let[t,r]=Ae(e),n=/^[ \t]*/.exec(t)[0],i=t.slice(n.length),a=n,l=Se.exec(i);if(l&&(a+=T("ref",l[1])+l[2],i=i.slice(l[0].length)),i){let d=Te.test(i)?"block":Ne.test(i)?"lit":Me.test(i)?"num":"str";a+=T(d,i)}return a+(r?T("comment",r):"")}function F(e){let t=/^[ \t]*/.exec(e)[0],r=e.slice(t.length),n=t;if(!r)return n;if(r==="---"||r==="...")return n+T("punct",r);let i=we.exec(r);if(i&&(n+=T("punct",i[0]),r=r.slice(i[0].length)),r.startsWith("#"))return n+T("comment",r);let a=Ce.exec(r);return a?(n+=T("key",a[1])+T("punct",":"),n+te(r.slice(a[1].length+1))):n+te(r)}function oe(e){return ne(e)}var s="imdx-jsonable",re=`
.${s} { font-family: ui-sans-serif, system-ui, sans-serif; font-size: 12px;
  color: #1a1d21; border: 1px solid #d8dde2; border-radius: 6px; overflow: hidden;
  background: #fff; box-sizing: border-box; }
.${s} *, .${s} *::before, .${s} *::after { box-sizing: border-box; }

.${s}-bar { display: flex; align-items: center; gap: 8px; padding: 6px 10px;
  background: #fafbfc; border-bottom: 1px solid #d8dde2; }
.${s}-bar { cursor: pointer; user-select: none; }
.${s}-bar:hover { background: #f6f8fa; }
.${s}-bar:focus-visible { outline: 2px solid #b7c0c9; outline-offset: -2px; }
.${s}-collapsed .${s}-bar { border-bottom: 0; }
.${s}-collapsed .${s}-scroll { display: none; }
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
`),t}function ae(e,t){if(!(e.children.length>0||e.block.length>0)){let d=document.createElement("div");return d.className=`${s}-line`,d.append(se(),ie(F(e.text))),d}let n=document.createElement("details");n.className=`${s}-fold`,n.open=t<_e;let i=document.createElement("summary");i.className=`${s}-line`,i.append(se(),ie(F(e.text)));let a=document.createElement("span");a.className=`${s}-count`;let l=q(e);a.textContent=`\u2026${l} line${l===1?"":"s"}`,i.appendChild(a),n.appendChild(i),e.block.length&&n.appendChild(ze(e.block));for(let d of e.children)n.appendChild(ae(d,t+1));return n}function P(e,t,r=""){e.innerHTML="",e.classList.add(s);let n=document.createElement("style");if(n.textContent=re,e.appendChild(n),!t||!t.trim()){let d=document.createElement("div");d.className=`${s}-placeholder`,d.textContent="Nothing to show.",e.appendChild(d);return}let i=ee(t),a=document.createElement("div");a.className=`${s}-doc`;for(let d of i)a.appendChild(ae(d,0));let l=document.createElement("div");l.className=`${s}-scroll`,l.appendChild(a),e.appendChild(Re(e,a,t,r)),e.appendChild(l)}function Re(e,t,r,n){let i=document.createElement("div");i.className=`${s}-bar`,i.tabIndex=0;let a=g=>{e.classList.toggle(`${s}-collapsed`,g),i.setAttribute("aria-expanded",String(!g)),i.title=g?"Expand":"Collapse"},l=()=>a(!e.classList.contains(`${s}-collapsed`));if(a(!1),i.addEventListener("click",l),i.addEventListener("keydown",g=>{g.target!==i||g.key!=="Enter"&&g.key!==" "||(g.preventDefault(),l())}),n){let g=document.createElement("span");g.className=`${s}-label`,g.textContent=n,i.appendChild(g)}let d=document.createElement("span");d.className=`${s}-meta`;let $=r.replace(/\n+$/,"").split(`
`).length;d.textContent=`${$.toLocaleString()} line${$===1?"":"s"}`,i.appendChild(d);let b=document.createElement("div");b.className=`${s}-actions`;let c=(g,N)=>{let w=document.createElement("button");return w.className=`${s}-btn`,w.type="button",w.textContent=g,w.addEventListener("click",_=>{_.stopPropagation(),N()}),b.appendChild(w),w},L=g=>{for(let N of t.querySelectorAll("details"))N.open=g};t.querySelector("details")&&(c("unfold all",()=>L(!0)),c("fold all",()=>L(!1)));let S=c("copy",()=>{navigator.clipboard?.writeText(r).then(()=>{S.textContent="copied",setTimeout(()=>S.textContent="copy",1200)})});return i.appendChild(b),i}var O="imdx-stack",Oe=`
.${O} { display: flex; flex-direction: column; gap: 8px; box-sizing: border-box; }
.${O}-placeholder { font-family: ui-sans-serif, system-ui, sans-serif;
  font-size: 12px; color: #9aa1a9; font-style: italic; padding: 10px;
  border: 1px solid #d8dde2; border-radius: 6px; background: #fff; }
`;function le(e,t){e.innerHTML="",e.classList.add(O);let r=document.createElement("style");r.textContent=Oe,e.appendChild(r);let n=()=>{let l=document.createElement("div");return e.appendChild(l),l},i=!!(t.pre&&t.pre.trim()),a=!!(t.post&&t.post.trim());if(i&&P(n(),t.pre),t.hasMain&&t.main(n()),a&&P(n(),t.post),!i&&!a&&!t.hasMain){let l=document.createElement("div");l.className=`${O}-placeholder`,l.textContent="Nothing to show.",e.appendChild(l)}}var de=new RegExp([/(?<str>'''[\s\S]*?'''|"""[\s\S]*?"""|'(?:[^'\\]|\\.)*'|"(?:[^"\\]|\\.)*")/,/(?<lit>\b(?:None|True|False)\b)/,/(?<cls>[A-Za-z_]\w*(?=\())/,/(?<attr>[A-Za-z_]\w*(?=\s*=))/,/(?<ident>[A-Za-z_]\w*)/,/(?<num>-?\d+(?:\.\d+)?)/].map(e=>e.source).join("|"),"g"),ce={str:"t-str",lit:"t-lit",cls:"t-cls",attr:"t-attr",num:"t-num"};function I(e){return e.replace(/[&<>]/g,t=>t==="&"?"&amp;":t==="<"?"&lt;":"&gt;")}function pe(e){let t="",r=0;for(let n=de.exec(e);n;n=de.exec(e)){t+=I(e.slice(r,n.index));let i=n.groups??{},a=Object.keys(ce).find(l=>i[l]!==void 0);t+=a?`<span class="${ce[a]}">${I(n[0])}</span>`:I(n[0]),r=n.index+n[0].length}return t+=I(e.slice(r)),t}var o="imdx-task",fe=`
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
`;var Ie=["debug","info","warning","error"],Ye={error:"\u274C",warning:"\u26A0\uFE0F",info:"\u2705",debug:"\u{1F4A1}"},Ke=4,Be="info";function H(e){return Ie.indexOf(e)}function A(e){return e.level??"info"}function De(e){let[t,r,...n]=e.split(":");return n.length?`${t}:${r}:${n.join(":").slice(0,6)}`:e}function u(e,t,r){let n=document.createElement(e);return t&&(n.className=`${o}-${t}`),r!==void 0&&(n.textContent=r),n}function ue(e,t){let r=u("td",e),n=u("div","descr"),i=u("span","descr-text",t||"\u2014");return t||i.classList.add(`${o}-descr-none`),n.appendChild(i),r.appendChild(n),r}var j=new WeakMap,me=e=>e.getClientRects().length>0;function ge(e){for(let t of e.querySelectorAll(`.${o}-scroll`))me(t)&&j.set(t,{top:t.scrollTop,left:t.scrollLeft})}function he(e){for(let t of e.querySelectorAll(`.${o}-scroll`)){let r=j.get(t);!r||!me(t)||(t.scrollTop=r.top,t.scrollLeft=r.left,j.delete(t))}}function qe(e,t){let r=u("div","art"),n=u("div","art-head");n.tabIndex=0;let i=c=>{c&&ge(r),r.classList.toggle(`${o}-art-collapsed`,c),c||he(r),n.setAttribute("aria-expanded",String(!c)),n.title=c?"Expand":"Collapse"},a=()=>i(!r.classList.contains(`${o}-art-collapsed`));i(!1),n.addEventListener("click",a),n.addEventListener("keydown",c=>{c.target!==n||c.key!=="Enter"&&c.key!==" "||(c.preventDefault(),a())}),n.appendChild(u("span","art-kind",e.kind)),n.appendChild(u("span","meta",`${e.repr.length.toLocaleString()} chars`));let l=u("button","copy","copy");l.type="button",l.title="Copy",l.addEventListener("click",c=>{c.stopPropagation(),navigator.clipboard?.writeText(e.repr).then(()=>{l.textContent="copied",setTimeout(()=>l.textContent="copy",1200)})}),n.appendChild(l);let d=u("button","close","\xD7");d.type="button",d.title="Remove",d.setAttribute("aria-label",`Remove ${e.kind}`),d.addEventListener("click",c=>{c.stopPropagation(),t()}),n.appendChild(d),r.appendChild(n);let $=u("div","scroll"),b=u("pre","pre");return b.innerHTML=pe(e.repr),$.appendChild(b),r.appendChild($),r}function be(e,t){e.innerHTML="",e.classList.add(o);let r=document.createElement("style");if(r.textContent=fe,e.appendChild(r),!t||t.length===0){e.appendChild(u("div","placeholder","No tasks."));return}let n=new Map;t.forEach((p,f)=>{let y=p.from_sym??"";n.has(y)||n.set(y,f)});let i=t.map((p,f)=>({t:p,i:f})).sort((p,f)=>H(A(f.t))-H(A(p.t))||n.get(p.t.from_sym??"")-n.get(f.t.from_sym??"")||p.i-f.i),a=new Map;for(let{t:p,i:f}of i){let y=H(A(p))>=H("warning");a.set(f,new Set(y?p.artifacts.map(m=>m.kind):[]))}let l=u("table","table"),d=document.createElement("thead"),$=document.createElement("tr");for(let p of["task","result","symbol","artifacts","kind"])$.appendChild(u("th",void 0,p));let b=u("th",void 0),c=u("div","id-head");c.appendChild(u("span",void 0,"id"));let L=u("label","show-debug"),S=document.createElement("input");S.type="checkbox",S.addEventListener("change",()=>_()),t.some(p=>A(p)==="debug")||(L.classList.add(`${o}-show-debug-none`),L.title="No debug tasks"),L.append(S,"show debug"),c.appendChild(L),b.appendChild(c),$.appendChild(b),d.appendChild($),l.appendChild(d);let g=document.createElement("tbody");l.appendChild(g),e.appendChild(l);let N=u("div","placeholder","No tasks at info level or above.");e.appendChild(N);let w=new Map;for(let{t:p,i:f}of i)w.set(f,xe(p,a.get(f)));function _(){g.innerHTML="";let p=H(S.checked?"debug":Be),f=i.filter(({t:y})=>H(A(y))>=p);N.hidden=f.length>0;for(let{i:y}of f){let{row:m,detail:E}=w.get(y);g.appendChild(m),a.get(y).size>0&&g.appendChild(E)}}function xe(p,f){let y=A(p),m=u("tr","row");m.dataset.level=y;let E=u("tr","detail"),z=document.createElement("td");z.colSpan=6,E.appendChild(z);let Y=new Map,J=new Map,C=!1,R=()=>{for(let[k,v]of J)v.setAttribute("aria-pressed",String(f.has(k)));f.size===0&&(C=!1),m.classList.toggle(`${o}-row-folded`,C),p.artifacts.length>0&&m.setAttribute("aria-expanded",String(f.size>0&&!C));let x=null;for(let k of p.artifacts){let v=Y.get(k.kind);if(!f.has(k.kind)){v?.remove(),Y.delete(k.kind);continue}v||(v=qe(k,()=>{f.delete(k.kind),R()}),Y.set(k.kind,v));let M=x?x.nextSibling:z.firstChild;M!==v&&z.insertBefore(v,M),x=v}C&&!E.hidden&&ge(E);let h=!C&&E.hidden;E.hidden=C,f.size===0?E.remove():m.parentNode&&m.nextSibling!==E&&m.after(E),h&&he(E)},U=()=>{if(f.size===0)for(let x of p.artifacts)f.add(x.kind);else C=!C;R()};if(p.artifacts.length>0){m.classList.add(`${o}-row-toggle`),m.tabIndex=0,m.title="Show / hide artifacts";let x=null;m.addEventListener("mousedown",h=>x={x:h.clientX,y:h.clientY}),m.addEventListener("click",h=>{let k=x;if(x=null,!((k?Math.hypot(h.clientX-k.x,h.clientY-k.y)>Ke:!1)||h.detail>2)){if(h.detail===1){let M=window.getSelection();M&&!M.isCollapsed&&m.contains(M.anchorNode)&&M.removeAllRanges()}U()}}),m.addEventListener("keydown",h=>{h.target!==m||h.key!=="Enter"&&h.key!==" "||(h.preventDefault(),U())})}m.appendChild(ue("task-descr",p.task_descr));let X=ue("res-descr",p.res_descr),V=u("span","level",Ye[y]);V.title=y,X.firstElementChild.appendChild(V),m.appendChild(X);let Z=u("td","sym",p.from_sym??"\u2014");p.from_sym==null&&Z.classList.add(`${o}-sym-none`),m.appendChild(Z);let G=u("td","chips");for(let x of p.artifacts){let h=u("button","chip",x.kind);h.type="button",h.addEventListener("click",k=>{k.stopPropagation(),f.has(x.kind)?f.delete(x.kind):f.add(x.kind),C=!1,R()}),J.set(x.kind,h),G.appendChild(h)}m.appendChild(G),m.appendChild(u("td","kind",p.kind.replace(/^TASK_/,"")));let W=u("td","id",De(p.id));return W.title=p.id,m.appendChild(W),R(),{row:m,detail:E}}_()}var Fe=["task_entries","pre","post"],it={render({model:e,el:t}){let r=()=>{let n=e.get("task_entries");le(t,{pre:e.get("pre"),post:e.get("post"),main:i=>be(i,n??[]),hasMain:n!=null})};r();for(let n of Fe)e.on(`change:${n}`,r)}};export{it as default};
