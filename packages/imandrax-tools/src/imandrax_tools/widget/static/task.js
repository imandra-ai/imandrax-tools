var z=/^\s*$/,me=/^(?:-(?:\s+|$))+/,ue=/:[ \t]*(?:#.*)?$/,ge=/(?:^[ \t]*|[:-][ \t]+)[|>][+-]?\d{0,2}[ \t]*(?:#.*)?$/;function O(e){return/^ */.exec(e)[0].length}function R(e,t){return me.exec(e.slice(t))?.[0].length??0}function he(e,t){return t+Math.max(1,R(e,t))}function be(e,t){return R(e,t)===0&&ue.test(e)?t:1/0}function xe(e){return ge.test(e)}function Z(e){return{text:e,indent:O(e),children:[],block:[]}}function G(e){let t=e.replace(/\n+$/,"").split(`
`),r=[],n=[],s=(a,l,c)=>{let b=n.length?n[n.length-1].node:null;(b?b.children:r).push(a),n.push({node:a,childIndent:l,seqIndent:c})};for(let a=0;a<t.length;a++){let l=t[a];if(z.test(l)){let g=Z(l);n.length?n[n.length-1].node.children.push(g):r.push(g);continue}let c=O(l),b=R(l,c)>0;for(;n.length;){let g=n[n.length-1],y=b?Math.min(g.childIndent,g.seqIndent):g.childIndent;if(c>=y)break;n.pop()}let x=Z(l);if(s(x,he(l,c),be(l,c)),!!xe(l)){for(;a+1<t.length;){let g=t[a+1];if(!z.test(g)&&O(g)<=c)break;x.block.push(g),a++}for(;x.block.length&&z.test(x.block[x.block.length-1]);)x.block.pop(),a--;n.pop()}}return r}function Y(e){let t=e.block.length;for(let r of e.children)t+=1+Y(r);return t}var $e=/^(?:-(?:[ \t]+|$))+/,ke=/^("(?:[^"\\]|\\.)*"|'(?:[^']|'')*'|[^:#\s][^:]*?)(:)([ \t]|$)/,ye=/^[|>][+-]?\d{0,2}$/,we=/^([&*]\S+|!!?\S*)([ \t]+|$)/,Ee=/^-?(?:\d[\d_]*(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$|^-?0[xXoObB][0-9a-fA-F_]+$|^[-+]?\.(?:inf|Inf|INF)$|^\.(?:nan|NaN|NAN)$/,Le=/^(?:true|True|TRUE|false|False|FALSE|null|Null|NULL|~)$/,Ce=/^"(?:[^"\\]|\\.)*"|^'(?:[^']|'')*'/;function W(e){return e.replace(/[&<>]/g,t=>t==="&"?"&amp;":t==="<"?"&lt;":"&gt;")}function E(e,t){return`<span class="t-${e}">${W(t)}</span>`}function ve(e){let t=/^[ \t]*/.exec(e)[0].length,r=Ce.exec(e.slice(t)),n=t+(r?r[0].length:0),s=/(?:^|[ \t])#/.exec(e.slice(n));if(!s)return[e,""];let a=n+s.index;return[e.slice(0,a),e.slice(a)]}function Q(e){let[t,r]=ve(e),n=/^[ \t]*/.exec(t)[0],s=t.slice(n.length),a=n,l=we.exec(s);if(l&&(a+=E("ref",l[1])+l[2],s=s.slice(l[0].length)),s){let c=ye.test(s)?"block":Le.test(s)?"lit":Ee.test(s)?"num":"str";a+=E(c,s)}return a+(r?E("comment",r):"")}function I(e){let t=/^[ \t]*/.exec(e)[0],r=e.slice(t.length),n=t;if(!r)return n;if(r==="---"||r==="...")return n+E("punct",r);let s=$e.exec(r);if(s&&(n+=E("punct",s[0]),r=r.slice(s[0].length)),r.startsWith("#"))return n+E("comment",r);let a=ke.exec(r);return a?(n+=E("key",a[1])+E("punct",":"),n+Q(r.slice(a[1].length+1))):n+Q(r)}function ee(e){return W(e)}var i="imdx-jsonable",te=`
.${i} { font-family: ui-sans-serif, system-ui, sans-serif; font-size: 12px;
  color: #1a1d21; border: 1px solid #d8dde2; border-radius: 6px; overflow: hidden;
  background: #fff; box-sizing: border-box; }
.${i} *, .${i} *::before, .${i} *::after { box-sizing: border-box; }

.${i}-bar { display: flex; align-items: center; gap: 8px; padding: 6px 10px;
  background: #fafbfc; border-bottom: 1px solid #d8dde2; }
.${i}-label { font-weight: 600; letter-spacing: 0.02em; }
.${i}-meta { color: #6b727b; font-size: 11px; font-variant-numeric: tabular-nums; }
.${i}-actions { margin-left: auto; display: flex; gap: 6px; }
.${i}-btn { font: inherit; font-size: 11px; color: #6b727b; background: transparent;
  border: 1px solid #d8dde2; border-radius: 4px; padding: 1px 6px; cursor: pointer; }
.${i}-btn:hover { color: #1a1d21; border-color: #b7c0c9; }

.${i}-scroll { max-height: 720px; overflow: auto; padding: 8px 0; }
.${i}-doc { font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 12px; line-height: 1.5; tab-size: 2; }

.${i}-line { display: flex; align-items: baseline; padding: 0 10px 0 4px; }
.${i}-line:hover { background: #f4f6f8; }
summary.${i}-line { cursor: pointer; user-select: none; list-style: none; }
summary.${i}-line::-webkit-details-marker { display: none; }

/* The fold gutter: same width on foldable and leaf lines, so text stays aligned. */
.${i}-arrow { flex: 0 0 1.1em; color: #9aa1a9; font-size: 9px; line-height: 1.7;
  text-align: center; }
summary.${i}-line > .${i}-arrow::before { content: "\\25B8"; display: inline-block;
  transition: transform 0.12s ease; }
details[open] > summary.${i}-line > .${i}-arrow::before { transform: rotate(90deg); }
summary.${i}-line:hover > .${i}-arrow { color: #1a1d21; }

.${i}-text { white-space: pre; }
.${i}-count { margin-left: 10px; color: #9aa1a9; font-size: 11px; font-style: italic;
  font-variant-numeric: tabular-nums; }
details[open] > summary > .${i}-count { display: none; }

/* Block-scalar bodies (\`key: |\`) \u2014 opaque text, dimmed and rendered verbatim. */
.${i}-block { margin: 0; padding: 0 10px 0 calc(1.1em + 4px); white-space: pre;
  color: #3c4249; }

/* Token colors (see jsonable/highlight.ts); light palette tuned for the #fff bg. */
.${i}-text .t-key { color: #0550ae; }      /* mapping keys */
.${i}-text .t-str { color: #0a7d33; }      /* quoted and plain scalars */
.${i}-text .t-num { color: #953800; }      /* numbers */
.${i}-text .t-lit { color: #cf222e; }      /* true / false / null / ~ */
.${i}-text .t-punct { color: #6b727b; }    /* \`-\`, \`:\`, \`---\` */
.${i}-text .t-ref { color: #8250df; }      /* anchors / aliases / tags */
.${i}-text .t-block { color: #8250df; }    /* \`|\` / \`>\` indicators */
.${i}-text .t-comment { color: #9aa1a9; font-style: italic; }

.${i}-placeholder { color: #9aa1a9; font-style: italic; padding: 10px; }
`;var Te=3;function ne(){let e=document.createElement("span");return e.className=`${i}-arrow`,e}function oe(e){let t=document.createElement("span");return t.className=`${i}-text`,t.innerHTML=e,t}function Me(e){let t=document.createElement("div");return t.className=`${i}-block`,t.innerHTML=e.map(ee).join(`
`),t}function re(e,t){if(!(e.children.length>0||e.block.length>0)){let c=document.createElement("div");return c.className=`${i}-line`,c.append(ne(),oe(I(e.text))),c}let n=document.createElement("details");n.className=`${i}-fold`,n.open=t<Te;let s=document.createElement("summary");s.className=`${i}-line`,s.append(ne(),oe(I(e.text)));let a=document.createElement("span");a.className=`${i}-count`;let l=Y(e);a.textContent=`\u2026${l} line${l===1?"":"s"}`,s.appendChild(a),n.appendChild(s),e.block.length&&n.appendChild(Me(e.block));for(let c of e.children)n.appendChild(re(c,t+1));return n}function K(e,t,r=""){e.innerHTML="",e.classList.add(i);let n=document.createElement("style");if(n.textContent=te,e.appendChild(n),!t||!t.trim()){let c=document.createElement("div");c.className=`${i}-placeholder`,c.textContent="Nothing to show.",e.appendChild(c);return}let s=G(t),a=document.createElement("div");a.className=`${i}-doc`;for(let c of s)a.appendChild(re(c,0));let l=document.createElement("div");l.className=`${i}-scroll`,l.appendChild(a),e.appendChild(Se(a,t,r)),e.appendChild(l)}function Se(e,t,r){let n=document.createElement("div");if(n.className=`${i}-bar`,r){let g=document.createElement("span");g.className=`${i}-label`,g.textContent=r,n.appendChild(g)}let s=document.createElement("span");s.className=`${i}-meta`;let a=t.replace(/\n+$/,"").split(`
`).length;s.textContent=`${a.toLocaleString()} line${a===1?"":"s"}`,n.appendChild(s);let l=document.createElement("div");l.className=`${i}-actions`;let c=(g,y)=>{let k=document.createElement("button");return k.className=`${i}-btn`,k.type="button",k.textContent=g,k.addEventListener("click",y),l.appendChild(k),k},b=g=>{for(let y of e.querySelectorAll("details"))y.open=g};c("expand all",()=>b(!0)),c("collapse all",()=>b(!1));let x=c("copy",()=>{navigator.clipboard?.writeText(t).then(()=>{x.textContent="copied",setTimeout(()=>x.textContent="copy",1200)})});return n.appendChild(l),n}var N="imdx-stack",Ne=`
.${N} { display: flex; flex-direction: column; gap: 8px; box-sizing: border-box; }
.${N}-placeholder { font-family: ui-sans-serif, system-ui, sans-serif;
  font-size: 12px; color: #9aa1a9; font-style: italic; padding: 10px;
  border: 1px solid #d8dde2; border-radius: 6px; background: #fff; }
`;function ie(e,t){e.innerHTML="",e.classList.add(N);let r=document.createElement("style");r.textContent=Ne,e.appendChild(r);let n=()=>{let l=document.createElement("div");return e.appendChild(l),l},s=!!(t.pre&&t.pre.trim()),a=!!(t.post&&t.post.trim());if(s&&K(n(),t.pre),t.hasMain&&t.main(n()),a&&K(n(),t.post),!s&&!a&&!t.hasMain){let l=document.createElement("div");l.className=`${N}-placeholder`,l.textContent="Nothing to show.",e.appendChild(l)}}var se=new RegExp([/(?<str>'''[\s\S]*?'''|"""[\s\S]*?"""|'(?:[^'\\]|\\.)*'|"(?:[^"\\]|\\.)*")/,/(?<lit>\b(?:None|True|False)\b)/,/(?<cls>[A-Za-z_]\w*(?=\())/,/(?<attr>[A-Za-z_]\w*(?=\s*=))/,/(?<ident>[A-Za-z_]\w*)/,/(?<num>-?\d+(?:\.\d+)?)/].map(e=>e.source).join("|"),"g"),ae={str:"t-str",lit:"t-lit",cls:"t-cls",attr:"t-attr",num:"t-num"};function H(e){return e.replace(/[&<>]/g,t=>t==="&"?"&amp;":t==="<"?"&lt;":"&gt;")}function le(e){let t="",r=0;for(let n=se.exec(e);n;n=se.exec(e)){t+=H(e.slice(r,n.index));let s=n.groups??{},a=Object.keys(ae).find(l=>s[l]!==void 0);t+=a?`<span class="${ae[a]}">${H(n[0])}</span>`:H(n[0]),r=n.index+n[0].length}return t+=H(e.slice(r)),t}var o="imdx-task",de=`
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

.${o}-detail > td { padding: 0 10px 8px; }
.${o}-detail > td > * + * { margin-top: 6px; }
.${o}-art { border: 1px solid #d8dde2; border-radius: 6px; overflow: hidden;
  background: #fff; }
.${o}-art-head { display: flex; align-items: center; gap: 8px; padding: 4px 10px;
  cursor: pointer; user-select: none; }
.${o}-art-head:hover { background: #f6f8fa; }
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
`;var He=["debug","info","warning","error"],_e={error:"\u274C",warning:"\u26A0\uFE0F",info:"\u2705",debug:"\u{1F4A1}"},Ae=4,ze="info";function L(e){return He.indexOf(e)}function C(e){return e.level??"info"}function Oe(e){let[t,r,...n]=e.split(":");return n.length?`${t}:${r}:${n.join(":").slice(0,6)}`:e}function m(e,t,r){let n=document.createElement(e);return t&&(n.className=`${o}-${t}`),r!==void 0&&(n.textContent=r),n}function ce(e,t){let r=m("td",e),n=m("div","descr"),s=m("span","descr-text",t||"\u2014");return t||s.classList.add(`${o}-descr-none`),n.appendChild(s),r.appendChild(n),r}function Re(e,t){let r=m("div","art"),n=m("div","art-head");n.title="Close",n.addEventListener("click",t),n.appendChild(m("span","art-kind",e.kind)),n.appendChild(m("span","meta",`${e.repr.length.toLocaleString()} chars`));let s=m("button","copy","copy");s.type="button",s.title="Copy",s.addEventListener("click",b=>{b.stopPropagation(),navigator.clipboard?.writeText(e.repr).then(()=>{s.textContent="copied",setTimeout(()=>s.textContent="copy",1200)})}),n.appendChild(s);let a=m("button","close","\xD7");a.type="button",a.setAttribute("aria-label",`Close ${e.kind}`),n.appendChild(a),r.appendChild(n);let l=m("div","scroll"),c=m("pre","pre");return c.innerHTML=le(e.repr),l.appendChild(c),r.appendChild(l),r}function pe(e,t){e.innerHTML="",e.classList.add(o);let r=document.createElement("style");if(r.textContent=de,e.appendChild(r),!t||t.length===0){e.appendChild(m("div","placeholder","No tasks."));return}let n=new Map;t.forEach((d,p)=>{let $=d.from_sym??"";n.has($)||n.set($,p)});let s=t.map((d,p)=>({t:d,i:p})).sort((d,p)=>L(C(p.t))-L(C(d.t))||n.get(d.t.from_sym??"")-n.get(p.t.from_sym??"")||d.i-p.i),a=new Map;for(let{t:d,i:p}of s){let $=L(C(d))>=L("warning");a.set(p,new Set($?d.artifacts.map(u=>u.kind):[]))}let l=m("table","table"),c=document.createElement("thead"),b=document.createElement("tr");for(let d of["task","result","symbol","artifacts","kind"])b.appendChild(m("th",void 0,d));let x=m("th",void 0),g=m("div","id-head");g.appendChild(m("span",void 0,"id"));let y=m("label","show-debug"),k=document.createElement("input");k.type="checkbox",k.addEventListener("change",()=>F()),t.some(d=>C(d)==="debug")||(y.classList.add(`${o}-show-debug-none`),y.title="No debug tasks"),y.append(k,"show debug"),g.appendChild(y),x.appendChild(g),b.appendChild(x),c.appendChild(b),l.appendChild(c);let T=document.createElement("tbody");l.appendChild(T),e.appendChild(l);let B=m("div","placeholder","No tasks at info level or above.");e.appendChild(B);let D=new Map;for(let{t:d,i:p}of s)D.set(p,fe(d,a.get(p)));function F(){T.innerHTML="";let d=L(k.checked?"debug":ze),p=s.filter(({t:$})=>L(C($))>=d);B.hidden=p.length>0;for(let{i:$}of p){let{row:u,detail:w}=D.get($);T.appendChild(u),w.hidden||T.appendChild(w)}}function fe(d,p){let $=C(d),u=m("tr","row");u.dataset.level=$;let w=m("tr","detail"),_=document.createElement("td");_.colSpan=6,w.appendChild(_);let A=new Map,q=new Map,M=()=>{for(let[h,f]of q)f.setAttribute("aria-pressed",String(p.has(h)));d.artifacts.length>0&&u.setAttribute("aria-expanded",String(p.size>0));for(let h of d.artifacts){let f=A.get(h.kind);if(!p.has(h.kind)){f?.remove(),A.delete(h.kind);continue}f||(f=Re(h,()=>{p.delete(h.kind),M()}),A.set(h.kind,f)),_.appendChild(f)}w.hidden=p.size===0,w.hidden?w.remove():u.parentNode&&u.nextSibling!==w&&u.after(w)},P=()=>{let h=p.size===d.artifacts.length;if(p.clear(),!h)for(let f of d.artifacts)p.add(f.kind);M()};if(d.artifacts.length>0){u.classList.add(`${o}-row-toggle`),u.tabIndex=0,u.title="Toggle all artifacts";let h=null;u.addEventListener("mousedown",f=>h={x:f.clientX,y:f.clientY}),u.addEventListener("click",f=>{let v=h;if(h=null,!((v?Math.hypot(f.clientX-v.x,f.clientY-v.y)>Ae:!1)||f.detail>2)){if(f.detail===1){let S=window.getSelection();S&&!S.isCollapsed&&u.contains(S.anchorNode)&&S.removeAllRanges()}P()}}),u.addEventListener("keydown",f=>{f.target!==u||f.key!=="Enter"&&f.key!==" "||(f.preventDefault(),P())})}u.appendChild(ce("task-descr",d.task_descr));let j=ce("res-descr",d.res_descr),J=m("span","level",_e[$]);J.title=$,j.firstElementChild.appendChild(J),u.appendChild(j);let U=m("td","sym",d.from_sym??"\u2014");d.from_sym==null&&U.classList.add(`${o}-sym-none`),u.appendChild(U);let X=m("td","chips");for(let h of d.artifacts){let f=m("button","chip",h.kind);f.type="button",f.addEventListener("click",v=>{v.stopPropagation(),p.has(h.kind)?p.delete(h.kind):p.add(h.kind),M()}),q.set(h.kind,f),X.appendChild(f)}u.appendChild(X),u.appendChild(m("td","kind",d.kind.replace(/^TASK_/,"")));let V=m("td","id",Oe(d.id));return V.title=d.id,u.appendChild(V),M(),{row:u,detail:w}}F()}var Ye=["task_entries","pre","post"],tt={render({model:e,el:t}){let r=()=>{let n=e.get("task_entries");ie(t,{pre:e.get("pre"),post:e.get("post"),main:s=>pe(s,n??[]),hasMain:n!=null})};r();for(let n of Ye)e.on(`change:${n}`,r)}};export{tt as default};
