var I=/^\s*$/,ue=/^(?:-(?:\s+|$))+/,he=/:[ \t]*(?:#.*)?$/,ge=/(?:^[ \t]*|[:-][ \t]+)[|>][+-]?\d{0,2}[ \t]*(?:#.*)?$/;function K(e){return/^ */.exec(e)[0].length}function B(e,t){return ue.exec(e.slice(t))?.[0].length??0}function be(e,t){return t+Math.max(1,B(e,t))}function xe(e,t){return B(e,t)===0&&he.test(e)?t:1/0}function $e(e){return ge.test(e)}function W(e){return{text:e,indent:K(e),children:[],block:[]}}function X(e){let t=e.replace(/\n+$/,"").split(`
`),r=[],n=[],a=(s,l,c)=>{let g=n.length?n[n.length-1].node:null;(g?g.children:r).push(s),n.push({node:s,childIndent:l,seqIndent:c})};for(let s=0;s<t.length;s++){let l=t[s];if(I.test(l)){let p=W(l);n.length?n[n.length-1].node.children.push(p):r.push(p);continue}let c=K(l),g=B(l,c)>0;for(;n.length;){let p=n[n.length-1],$=g?Math.min(p.childIndent,p.seqIndent):p.childIndent;if(c>=$)break;n.pop()}let b=W(l);if(a(b,be(l,c),xe(l,c)),!!$e(l)){for(;s+1<t.length;){let p=t[s+1];if(!I.test(p)&&K(p)<=c)break;b.block.push(p),s++}for(;b.block.length&&I.test(b.block[b.block.length-1]);)b.block.pop(),s--;n.pop()}}return r}function F(e){let t=e.block.length;for(let r of e.children)t+=1+F(r);return t}var ye=/^(?:-(?:[ \t]+|$))+/,ke=/^("(?:[^"\\]|\\.)*"|'(?:[^']|'')*'|[^:#\s][^:]*?)(:)([ \t]|$)/,Ee=/^[|>][+-]?\d{0,2}$/,Ce=/^([&*]\S+|!!?\S*)([ \t]+|$)/,Le=/^-?(?:\d[\d_]*(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$|^-?0[xXoObB][0-9a-fA-F_]+$|^[-+]?\.(?:inf|Inf|INF)$|^\.(?:nan|NaN|NAN)$/,we=/^(?:true|True|TRUE|false|False|FALSE|null|Null|NULL|~)$/,ve=/^"(?:[^"\\]|\\.)*"|^'(?:[^']|'')*'/;function te(e){return e.replace(/[&<>]/g,t=>t==="&"?"&amp;":t==="<"?"&lt;":"&gt;")}function E(e,t){return`<span class="t-${e}">${te(t)}</span>`}function Te(e){let t=/^[ \t]*/.exec(e)[0].length,r=ve.exec(e.slice(t)),n=t+(r?r[0].length:0),a=/(?:^|[ \t])#/.exec(e.slice(n));if(!a)return[e,""];let s=n+a.index;return[e.slice(0,s),e.slice(s)]}function ee(e){let[t,r]=Te(e),n=/^[ \t]*/.exec(t)[0],a=t.slice(n.length),s=n,l=Ce.exec(a);if(l&&(s+=E("ref",l[1])+l[2],a=a.slice(l[0].length)),a){let c=Ee.test(a)?"block":we.test(a)?"lit":Le.test(a)?"num":"str";s+=E(c,a)}return s+(r?E("comment",r):"")}function D(e){let t=/^[ \t]*/.exec(e)[0],r=e.slice(t.length),n=t;if(!r)return n;if(r==="---"||r==="...")return n+E("punct",r);let a=ye.exec(r);if(a&&(n+=E("punct",a[0]),r=r.slice(a[0].length)),r.startsWith("#"))return n+E("comment",r);let s=ke.exec(r);return s?(n+=E("key",s[1])+E("punct",":"),n+ee(r.slice(s[1].length+1))):n+ee(r)}function ne(e){return te(e)}var i="imdx-jsonable",oe=`
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
`;var Se=3;function re(){let e=document.createElement("span");return e.className=`${i}-arrow`,e}function ie(e){let t=document.createElement("span");return t.className=`${i}-text`,t.innerHTML=e,t}function Ne(e){let t=document.createElement("div");return t.className=`${i}-block`,t.innerHTML=e.map(ne).join(`
`),t}function se(e,t){if(!(e.children.length>0||e.block.length>0)){let c=document.createElement("div");return c.className=`${i}-line`,c.append(re(),ie(D(e.text))),c}let n=document.createElement("details");n.className=`${i}-fold`,n.open=t<Se;let a=document.createElement("summary");a.className=`${i}-line`,a.append(re(),ie(D(e.text)));let s=document.createElement("span");s.className=`${i}-count`;let l=F(e);s.textContent=`\u2026${l} line${l===1?"":"s"}`,a.appendChild(s),n.appendChild(a),e.block.length&&n.appendChild(Ne(e.block));for(let c of e.children)n.appendChild(se(c,t+1));return n}function q(e,t,r=""){e.innerHTML="",e.classList.add(i);let n=document.createElement("style");if(n.textContent=oe,e.appendChild(n),!t||!t.trim()){let c=document.createElement("div");c.className=`${i}-placeholder`,c.textContent="Nothing to show.",e.appendChild(c);return}let a=X(t),s=document.createElement("div");s.className=`${i}-doc`;for(let c of a)s.appendChild(se(c,0));let l=document.createElement("div");l.className=`${i}-scroll`,l.appendChild(s),e.appendChild(Me(s,t,r)),e.appendChild(l)}function Me(e,t,r){let n=document.createElement("div");if(n.className=`${i}-bar`,r){let p=document.createElement("span");p.className=`${i}-label`,p.textContent=r,n.appendChild(p)}let a=document.createElement("span");a.className=`${i}-meta`;let s=t.replace(/\n+$/,"").split(`
`).length;a.textContent=`${s.toLocaleString()} line${s===1?"":"s"}`,n.appendChild(a);let l=document.createElement("div");l.className=`${i}-actions`;let c=(p,$)=>{let x=document.createElement("button");return x.className=`${i}-btn`,x.type="button",x.textContent=p,x.addEventListener("click",$),l.appendChild(x),x},g=p=>{for(let $ of e.querySelectorAll("details"))$.open=p};c("expand all",()=>g(!0)),c("collapse all",()=>g(!1));let b=c("copy",()=>{navigator.clipboard?.writeText(t).then(()=>{b.textContent="copied",setTimeout(()=>b.textContent="copy",1200)})});return n.appendChild(l),n}var O="imdx-stack",_e=`
.${O} { display: flex; flex-direction: column; gap: 8px; box-sizing: border-box; }
.${O}-placeholder { font-family: ui-sans-serif, system-ui, sans-serif;
  font-size: 12px; color: #9aa1a9; font-style: italic; padding: 10px;
  border: 1px solid #d8dde2; border-radius: 6px; background: #fff; }
`;function ae(e,t){e.innerHTML="",e.classList.add(O);let r=document.createElement("style");r.textContent=_e,e.appendChild(r);let n=()=>{let l=document.createElement("div");return e.appendChild(l),l},a=!!(t.pre&&t.pre.trim()),s=!!(t.post&&t.post.trim());if(a&&q(n(),t.pre),t.hasMain&&t.main(n()),s&&q(n(),t.post),!a&&!s&&!t.hasMain){let l=document.createElement("div");l.className=`${O}-placeholder`,l.textContent="Nothing to show.",e.appendChild(l)}}var le=new RegExp([/(?<str>'''[\s\S]*?'''|"""[\s\S]*?"""|'(?:[^'\\]|\\.)*'|"(?:[^"\\]|\\.)*")/,/(?<lit>\b(?:None|True|False)\b)/,/(?<cls>[A-Za-z_]\w*(?=\())/,/(?<attr>[A-Za-z_]\w*(?=\s*=))/,/(?<ident>[A-Za-z_]\w*)/,/(?<num>-?\d+(?:\.\d+)?)/].map(e=>e.source).join("|"),"g"),ce={str:"t-str",lit:"t-lit",cls:"t-cls",attr:"t-attr",num:"t-num"};function R(e){return e.replace(/[&<>]/g,t=>t==="&"?"&amp;":t==="<"?"&lt;":"&gt;")}function de(e){let t="",r=0;for(let n=le.exec(e);n;n=le.exec(e)){t+=R(e.slice(r,n.index));let a=n.groups??{},s=Object.keys(ce).find(l=>a[l]!==void 0);t+=s?`<span class="${ce[s]}">${R(n[0])}</span>`:R(n[0]),r=n.index+n[0].length}return t+=R(e.slice(r)),t}var o="imdx-task",pe=`
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
.${o}-level { width: 24px; text-align: center; color: #9aa1a9; }
.${o}-sym { cursor: pointer; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-weight: 600; }
.${o}-sym-btn { font: inherit; color: inherit; background: none; border: 0;
  padding: 0; cursor: pointer; text-align: left; }
.${o}-sym-none { color: #9aa1a9; font-weight: 400; }
/* Same shade as an artifact title on hover. */
.${o}-table td.${o}-sym-hover { background: #f6f8fa; }
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

.${o}-detail > td { padding: 0 10px 8px 34px; }
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
`;var Ae=["debug","info","warning","error"],He={error:"\u274C",warning:"\u26A0\uFE0F",info:"\u2705",debug:"\u{1F4A1}"},ze="info";function N(e){return Ae.indexOf(e)}function M(e){return e.level??"debug"}function Oe(e){let[t,r,...n]=e.split(":");return n.length?`${t}:${r}:${n.join(":").slice(0,6)}`:e}function d(e,t,r){let n=document.createElement(e);return t&&(n.className=`${o}-${t}`),r!==void 0&&(n.textContent=r),n}function Re(e,t){let r=d("div","art"),n=d("div","art-head");n.title="Close",n.addEventListener("click",t),n.appendChild(d("span","art-kind",e.kind)),n.appendChild(d("span","meta",`${e.repr.length.toLocaleString()} chars`));let a=d("button","copy","copy");a.type="button",a.title="Copy",a.addEventListener("click",g=>{g.stopPropagation(),navigator.clipboard?.writeText(e.repr).then(()=>{a.textContent="copied",setTimeout(()=>a.textContent="copy",1200)})}),n.appendChild(a);let s=d("button","close","\xD7");s.type="button",s.setAttribute("aria-label",`Close ${e.kind}`),n.appendChild(s),r.appendChild(n);let l=d("div","scroll"),c=d("pre","pre");return c.innerHTML=de(e.repr),l.appendChild(c),r.appendChild(l),r}function fe(e,t){e.innerHTML="",e.classList.add(o);let r=document.createElement("style");if(r.textContent=pe,e.appendChild(r),!t||t.length===0){e.appendChild(d("div","placeholder","No tasks."));return}let n=new Map;t.forEach((f,m)=>{let S=f.from_sym??"";n.has(S)||n.set(S,m)});let a=t.map((f,m)=>({t:f,i:m})).sort((f,m)=>N(M(m.t))-N(M(f.t))||n.get(f.t.from_sym??"")-n.get(m.t.from_sym??"")||f.i-m.i),s=new Map;for(let{t:f,i:m}of a){let S=N(M(f))>=N("warning");s.set(m,new Set(S?f.artifacts.map(z=>z.kind):[]))}let l=d("table","table"),c=document.createElement("thead"),g=document.createElement("tr");for(let f of["","symbol","artifacts","kind"])g.appendChild(d("th",void 0,f));let b=d("th",void 0),p=d("div","id-head");p.appendChild(d("span",void 0,"id"));let $=d("label","show-debug"),x=document.createElement("input");x.type="checkbox",x.addEventListener("change",()=>_()),t.some(f=>M(f)==="debug")||($.classList.add(`${o}-show-debug-none`),$.title="No debug tasks"),$.append(x,"show debug"),p.appendChild($),b.appendChild(p),g.appendChild(b),c.appendChild(g),l.appendChild(c);let H=document.createElement("tbody");l.appendChild(H),e.appendChild(l);let P=d("div","placeholder","No tasks at info level or above.");e.appendChild(P);function _(){H.innerHTML="";let f=N(x.checked?"debug":ze),m=a.filter(({t:C})=>N(M(C))>=f);P.hidden=m.length>0;let S=C=>{let y=C.every(({t:A,i:L})=>s.get(L).size===A.artifacts.length);for(let{t:A,i:L}of C)s.set(L,new Set(y?[]:A.artifacts.map(k=>k.kind)));_()},z,j=[],J=[];for(let[C,{t:y,i:A}]of m.entries()){let L=M(y),k=d("tr","row");k.dataset.level=L;let U=d("td","level",He[L]);U.title=L,k.appendChild(U);let w=y.from_sym??null,v=d("td","sym");if(w===null||w!==z){let u=C+1;for(;w!==null&&u<m.length&&m[u].t.from_sym===w;)u++;j=m.slice(C,u),J=[];let h=d("button","sym-btn",w??"\u2014");h.type="button",w===null&&h.classList.add(`${o}-sym-none`),v.appendChild(h)}z=w;let[me,V]=[j,J];V.push(v),v.title="Toggle all artifacts",v.addEventListener("click",()=>S(me));let Z=u=>()=>V.forEach(h=>h.classList.toggle(`${o}-sym-hover`,u));v.addEventListener("mouseenter",Z(!0)),v.addEventListener("mouseleave",Z(!1)),k.appendChild(v);let G=d("td","chips"),T=s.get(A);for(let u of y.artifacts){let h=d("button","chip",u.kind);h.type="button",h.setAttribute("aria-pressed",String(T.has(u.kind))),h.addEventListener("click",()=>{T.has(u.kind)?T.delete(u.kind):T.add(u.kind),_()}),G.appendChild(h)}k.appendChild(G),k.appendChild(d("td","kind",y.kind.replace(/^TASK_/,"")));let Q=d("td","id",Oe(y.id));if(Q.title=y.id,k.appendChild(Q),H.appendChild(k),T.size>0){let u=d("tr","detail"),h=document.createElement("td");h.colSpan=5;for(let Y of y.artifacts)T.has(Y.kind)&&h.appendChild(Re(Y,()=>{T.delete(Y.kind),_()}));u.appendChild(h),H.appendChild(u)}}}_()}var Ye=["task_entries","pre","post"],et={render({model:e,el:t}){let r=()=>{let n=e.get("task_entries");ae(t,{pre:e.get("pre"),post:e.get("post"),main:a=>fe(a,n??[]),hasMain:n!=null})};r();for(let n of Ye)e.on(`change:${n}`,r)}};export{et as default};
