var O=/^\s*$/,ce=/^(?:-(?:\s+|$))+/,de=/:[ \t]*(?:#.*)?$/,pe=/(?:^[ \t]*|[:-][ \t]+)[|>][+-]?\d{0,2}[ \t]*(?:#.*)?$/;function R(e){return/^ */.exec(e)[0].length}function Y(e,t){return ce.exec(e.slice(t))?.[0].length??0}function fe(e,t){return t+Math.max(1,Y(e,t))}function me(e,t){return Y(e,t)===0&&de.test(e)?t:1/0}function ue(e){return pe.test(e)}function U(e){return{text:e,indent:R(e),children:[],block:[]}}function V(e){let t=e.replace(/\n+$/,"").split(`
`),o=[],n=[],a=(i,l,c)=>{let g=n.length?n[n.length-1].node:null;(g?g.children:o).push(i),n.push({node:i,childIndent:l,seqIndent:c})};for(let i=0;i<t.length;i++){let l=t[i];if(O.test(l)){let p=U(l);n.length?n[n.length-1].node.children.push(p):o.push(p);continue}let c=R(l),g=Y(l,c)>0;for(;n.length;){let p=n[n.length-1],x=g?Math.min(p.childIndent,p.seqIndent):p.childIndent;if(c>=x)break;n.pop()}let u=U(l);if(a(u,fe(l,c),me(l,c)),!!ue(l)){for(;i+1<t.length;){let p=t[i+1];if(!O.test(p)&&R(p)<=c)break;u.block.push(p),i++}for(;u.block.length&&O.test(u.block[u.block.length-1]);)u.block.pop(),i--;n.pop()}}return o}function I(e){let t=e.block.length;for(let o of e.children)t+=1+I(o);return t}var he=/^(?:-(?:[ \t]+|$))+/,ge=/^("(?:[^"\\]|\\.)*"|'(?:[^']|'')*'|[^:#\s][^:]*?)(:)([ \t]|$)/,be=/^[|>][+-]?\d{0,2}$/,xe=/^([&*]\S+|!!?\S*)([ \t]+|$)/,$e=/^-?(?:\d[\d_]*(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$|^-?0[xXoObB][0-9a-fA-F_]+$|^[-+]?\.(?:inf|Inf|INF)$|^\.(?:nan|NaN|NAN)$/,ye=/^(?:true|True|TRUE|false|False|FALSE|null|Null|NULL|~)$/,ke=/^"(?:[^"\\]|\\.)*"|^'(?:[^']|'')*'/;function G(e){return e.replace(/[&<>]/g,t=>t==="&"?"&amp;":t==="<"?"&lt;":"&gt;")}function k(e,t){return`<span class="t-${e}">${G(t)}</span>`}function Ee(e){let t=/^[ \t]*/.exec(e)[0].length,o=ke.exec(e.slice(t)),n=t+(o?o[0].length:0),a=/(?:^|[ \t])#/.exec(e.slice(n));if(!a)return[e,""];let i=n+a.index;return[e.slice(0,i),e.slice(i)]}function Z(e){let[t,o]=Ee(e),n=/^[ \t]*/.exec(t)[0],a=t.slice(n.length),i=n,l=xe.exec(a);if(l&&(i+=k("ref",l[1])+l[2],a=a.slice(l[0].length)),a){let c=be.test(a)?"block":ye.test(a)?"lit":$e.test(a)?"num":"str";i+=k(c,a)}return i+(o?k("comment",o):"")}function K(e){let t=/^[ \t]*/.exec(e)[0],o=e.slice(t.length),n=t;if(!o)return n;if(o==="---"||o==="...")return n+k("punct",o);let a=he.exec(o);if(a&&(n+=k("punct",a[0]),o=o.slice(a[0].length)),o.startsWith("#"))return n+k("comment",o);let i=ge.exec(o);return i?(n+=k("key",i[1])+k("punct",":"),n+Z(o.slice(i[1].length+1))):n+Z(o)}function Q(e){return G(e)}var r="imdx-jsonable",W=`
.${r} { font-family: ui-sans-serif, system-ui, sans-serif; font-size: 12px;
  color: #1a1d21; border: 1px solid #d8dde2; border-radius: 6px; overflow: hidden;
  background: #fff; box-sizing: border-box; }
.${r} *, .${r} *::before, .${r} *::after { box-sizing: border-box; }

.${r}-bar { display: flex; align-items: center; gap: 8px; padding: 6px 10px;
  background: #fafbfc; border-bottom: 1px solid #d8dde2; }
.${r}-label { font-weight: 600; letter-spacing: 0.02em; }
.${r}-meta { color: #6b727b; font-size: 11px; font-variant-numeric: tabular-nums; }
.${r}-actions { margin-left: auto; display: flex; gap: 6px; }
.${r}-btn { font: inherit; font-size: 11px; color: #6b727b; background: transparent;
  border: 1px solid #d8dde2; border-radius: 4px; padding: 1px 6px; cursor: pointer; }
.${r}-btn:hover { color: #1a1d21; border-color: #b7c0c9; }

.${r}-scroll { max-height: 720px; overflow: auto; padding: 8px 0; }
.${r}-doc { font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 12px; line-height: 1.5; tab-size: 2; }

.${r}-line { display: flex; align-items: baseline; padding: 0 10px 0 4px; }
.${r}-line:hover { background: #f4f6f8; }
summary.${r}-line { cursor: pointer; user-select: none; list-style: none; }
summary.${r}-line::-webkit-details-marker { display: none; }

/* The fold gutter: same width on foldable and leaf lines, so text stays aligned. */
.${r}-arrow { flex: 0 0 1.1em; color: #9aa1a9; font-size: 9px; line-height: 1.7;
  text-align: center; }
summary.${r}-line > .${r}-arrow::before { content: "\\25B8"; display: inline-block;
  transition: transform 0.12s ease; }
details[open] > summary.${r}-line > .${r}-arrow::before { transform: rotate(90deg); }
summary.${r}-line:hover > .${r}-arrow { color: #1a1d21; }

.${r}-text { white-space: pre; }
.${r}-count { margin-left: 10px; color: #9aa1a9; font-size: 11px; font-style: italic;
  font-variant-numeric: tabular-nums; }
details[open] > summary > .${r}-count { display: none; }

/* Block-scalar bodies (\`key: |\`) \u2014 opaque text, dimmed and rendered verbatim. */
.${r}-block { margin: 0; padding: 0 10px 0 calc(1.1em + 4px); white-space: pre;
  color: #3c4249; }

/* Token colors (see jsonable/highlight.ts); light palette tuned for the #fff bg. */
.${r}-text .t-key { color: #0550ae; }      /* mapping keys */
.${r}-text .t-str { color: #0a7d33; }      /* quoted and plain scalars */
.${r}-text .t-num { color: #953800; }      /* numbers */
.${r}-text .t-lit { color: #cf222e; }      /* true / false / null / ~ */
.${r}-text .t-punct { color: #6b727b; }    /* \`-\`, \`:\`, \`---\` */
.${r}-text .t-ref { color: #8250df; }      /* anchors / aliases / tags */
.${r}-text .t-block { color: #8250df; }    /* \`|\` / \`>\` indicators */
.${r}-text .t-comment { color: #9aa1a9; font-style: italic; }

.${r}-placeholder { color: #9aa1a9; font-style: italic; padding: 10px; }
`;var Le=3;function X(){let e=document.createElement("span");return e.className=`${r}-arrow`,e}function ee(e){let t=document.createElement("span");return t.className=`${r}-text`,t.innerHTML=e,t}function Ce(e){let t=document.createElement("div");return t.className=`${r}-block`,t.innerHTML=e.map(Q).join(`
`),t}function te(e,t){if(!(e.children.length>0||e.block.length>0)){let c=document.createElement("div");return c.className=`${r}-line`,c.append(X(),ee(K(e.text))),c}let n=document.createElement("details");n.className=`${r}-fold`,n.open=t<Le;let a=document.createElement("summary");a.className=`${r}-line`,a.append(X(),ee(K(e.text)));let i=document.createElement("span");i.className=`${r}-count`;let l=I(e);i.textContent=`\u2026${l} line${l===1?"":"s"}`,a.appendChild(i),n.appendChild(a),e.block.length&&n.appendChild(Ce(e.block));for(let c of e.children)n.appendChild(te(c,t+1));return n}function B(e,t,o=""){e.innerHTML="",e.classList.add(r);let n=document.createElement("style");if(n.textContent=W,e.appendChild(n),!t||!t.trim()){let c=document.createElement("div");c.className=`${r}-placeholder`,c.textContent="Nothing to show.",e.appendChild(c);return}let a=V(t),i=document.createElement("div");i.className=`${r}-doc`;for(let c of a)i.appendChild(te(c,0));let l=document.createElement("div");l.className=`${r}-scroll`,l.appendChild(i),e.appendChild(ve(i,t,o)),e.appendChild(l)}function ve(e,t,o){let n=document.createElement("div");if(n.className=`${r}-bar`,o){let p=document.createElement("span");p.className=`${r}-label`,p.textContent=o,n.appendChild(p)}let a=document.createElement("span");a.className=`${r}-meta`;let i=t.replace(/\n+$/,"").split(`
`).length;a.textContent=`${i.toLocaleString()} line${i===1?"":"s"}`,n.appendChild(a);let l=document.createElement("div");l.className=`${r}-actions`;let c=(p,x)=>{let d=document.createElement("button");return d.className=`${r}-btn`,d.type="button",d.textContent=p,d.addEventListener("click",x),l.appendChild(d),d},g=p=>{for(let x of e.querySelectorAll("details"))x.open=p};c("expand all",()=>g(!0)),c("collapse all",()=>g(!1));let u=c("copy",()=>{navigator.clipboard?.writeText(t).then(()=>{u.textContent="copied",setTimeout(()=>u.textContent="copy",1200)})});return n.appendChild(l),n}var A="imdx-stack",Te=`
.${A} { display: flex; flex-direction: column; gap: 8px; box-sizing: border-box; }
.${A}-placeholder { font-family: ui-sans-serif, system-ui, sans-serif;
  font-size: 12px; color: #9aa1a9; font-style: italic; padding: 10px;
  border: 1px solid #d8dde2; border-radius: 6px; background: #fff; }
`;function ne(e,t){e.innerHTML="",e.classList.add(A);let o=document.createElement("style");o.textContent=Te,e.appendChild(o);let n=()=>{let l=document.createElement("div");return e.appendChild(l),l},a=!!(t.pre&&t.pre.trim()),i=!!(t.post&&t.post.trim());if(a&&B(n(),t.pre),t.hasMain&&t.main(n()),i&&B(n(),t.post),!a&&!i&&!t.hasMain){let l=document.createElement("div");l.className=`${A}-placeholder`,l.textContent="Nothing to show.",e.appendChild(l)}}var oe=new RegExp([/(?<str>'''[\s\S]*?'''|"""[\s\S]*?"""|'(?:[^'\\]|\\.)*'|"(?:[^"\\]|\\.)*")/,/(?<lit>\b(?:None|True|False)\b)/,/(?<cls>[A-Za-z_]\w*(?=\())/,/(?<attr>[A-Za-z_]\w*(?=\s*=))/,/(?<ident>[A-Za-z_]\w*)/,/(?<num>-?\d+(?:\.\d+)?)/].map(e=>e.source).join("|"),"g"),re={str:"t-str",lit:"t-lit",cls:"t-cls",attr:"t-attr",num:"t-num"};function H(e){return e.replace(/[&<>]/g,t=>t==="&"?"&amp;":t==="<"?"&lt;":"&gt;")}function se(e){let t="",o=0;for(let n=oe.exec(e);n;n=oe.exec(e)){t+=H(e.slice(o,n.index));let a=n.groups??{},i=Object.keys(re).find(l=>a[l]!==void 0);t+=i?`<span class="${re[i]}">${H(n[0])}</span>`:H(n[0]),o=n.index+n[0].length}return t+=H(e.slice(o)),t}var s="imdx-task",ie=`
.${s} { display: flex; flex-direction: column; gap: 8px;
  font-family: ui-sans-serif, system-ui, sans-serif; font-size: 12px;
  color: #1a1d21; box-sizing: border-box; }
.${s} *, .${s} *::before, .${s} *::after { box-sizing: border-box; }

.${s}-table { width: 100%; border-collapse: collapse; border: 1px solid #d8dde2;
  border-radius: 6px; overflow: hidden; background: #fafbfc; }
.${s}-table th { text-align: left; font-weight: 600; color: #6b727b; font-size: 11px;
  padding: 5px 10px; border-bottom: 1px solid #d8dde2; background: #fff; }
.${s}-table td { padding: 4px 10px; vertical-align: middle; }
.${s}-row + .${s}-row > td,
.${s}-detail + .${s}-row > td { border-top: 1px solid #eef1f4; }
.${s}-row[data-level="error"] { background: #fff5f5; }
.${s}-row[data-level="warning"] { background: #fffaeb; }
.${s}-level { width: 24px; text-align: center; color: #9aa1a9; }
.${s}-sym { cursor: pointer; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-weight: 600; }
.${s}-sym-btn { font: inherit; color: inherit; background: none; border: 0;
  padding: 0; cursor: pointer; text-align: left; }
.${s}-sym-none { color: #9aa1a9; font-weight: 400; }
/* Same shade as an artifact title on hover. */
.${s}-table td.${s}-sym-hover { background: #f6f8fa; }
.${s}-kind { color: #6b727b; font-size: 11px; letter-spacing: 0.02em; }
.${s}-id { color: #9aa1a9; font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 11px; white-space: nowrap; }
.${s}-meta { color: #6b727b; font-size: 11px; font-variant-numeric: tabular-nums; }

.${s}-chips { display: flex; gap: 4px; flex-wrap: wrap; }
.${s}-chip { font: inherit; font-size: 11px; cursor: pointer; padding: 1px 7px;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  color: #6b727b; background: #fff; border: 1px solid #d8dde2; border-radius: 10px; }
.${s}-chip:hover { color: #1a1d21; border-color: #b7c0c9; }
.${s}-chip[aria-pressed="true"] { color: #1a1d21; background: #e3e8ee; border-color: #b7c0c9; }

.${s}-detail > td { padding: 0 10px 8px 34px; }
.${s}-detail > td > * + * { margin-top: 6px; }
.${s}-art { border: 1px solid #d8dde2; border-radius: 6px; overflow: hidden;
  background: #fff; }
.${s}-art-head { display: flex; align-items: center; gap: 8px; padding: 4px 10px;
  cursor: pointer; user-select: none; }
.${s}-art-head:hover { background: #f6f8fa; }
.${s}-art-kind { font-weight: 600; color: #1a1d21;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }

.${s}-copy { margin-left: auto; }
.${s}-copy, .${s}-close { font: inherit; font-size: 11px; color: #6b727b;
  background: transparent; border: 1px solid #d8dde2; border-radius: 4px; padding: 1px 6px;
  cursor: pointer; }
.${s}-copy:hover, .${s}-close:hover { color: #1a1d21; border-color: #b7c0c9; }

.${s}-scroll { max-height: 720px; overflow: auto; border-top: 1px solid #d8dde2; }
.${s}-pre { margin: 0; padding: 10px; white-space: pre; tab-size: 2; font-size: 12px;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }

/* Syntax highlighting for the Python-repr artifact text (see task/highlight.ts).
   Light palette tuned for the #fff code bg. */
.${s}-pre .t-cls { color: #8250df; }   /* constructor / class names */
.${s}-pre .t-attr { color: #0550ae; }  /* keyword-arg names */
.${s}-pre .t-str { color: #0a7d33; }   /* string literals */
.${s}-pre .t-num { color: #953800; }   /* numbers */
.${s}-pre .t-lit { color: #cf222e; }   /* None / True / False */

.${s}-placeholder { color: #9aa1a9; font-style: italic; padding: 8px; }
`;var we=["debug","info","warning","error"],Se={error:"\u274C",warning:"\u26A0\uFE0F",info:"\u2705",debug:"\u{1F4A1}"},Ne="info";function S(e){return we.indexOf(e)}function M(e){return e.level??"debug"}function Me(e){let[t,o,...n]=e.split(":");return n.length?`${t}:${o}:${n.join(":").slice(0,6)}`:e}function f(e,t,o){let n=document.createElement(e);return t&&(n.className=`${s}-${t}`),o!==void 0&&(n.textContent=o),n}function _e(e,t){let o=f("div","art"),n=f("div","art-head");n.title="Close",n.addEventListener("click",t),n.appendChild(f("span","art-kind",e.kind)),n.appendChild(f("span","meta",`${e.repr.length.toLocaleString()} chars`));let a=f("button","copy","copy");a.type="button",a.title="Copy",a.addEventListener("click",g=>{g.stopPropagation(),navigator.clipboard?.writeText(e.repr).then(()=>{a.textContent="copied",setTimeout(()=>a.textContent="copy",1200)})}),n.appendChild(a);let i=f("button","close","\xD7");i.type="button",i.setAttribute("aria-label",`Close ${e.kind}`),n.appendChild(i),o.appendChild(n);let l=f("div","scroll"),c=f("pre","pre");return c.innerHTML=se(e.repr),l.appendChild(c),o.appendChild(l),o}function ae(e,t){e.innerHTML="",e.classList.add(s);let o=document.createElement("style");if(o.textContent=ie,e.appendChild(o),!t||t.length===0){e.appendChild(f("div","placeholder","No tasks."));return}let n=new Map;t.forEach((d,b)=>{let E=d.from_sym??"";n.has(E)||n.set(E,b)});let a=t.map((d,b)=>({t:d,i:b})).sort((d,b)=>S(M(b.t))-S(M(d.t))||n.get(d.t.from_sym??"")-n.get(b.t.from_sym??"")||d.i-b.i),i=new Map;for(let{t:d,i:b}of a){let E=S(M(d))>=S("warning");i.set(b,new Set(E?d.artifacts.map(_=>_.kind):[]))}let l=f("table","table"),c=document.createElement("thead"),g=document.createElement("tr");for(let d of["","symbol","kind","artifacts","id"])g.appendChild(f("th",void 0,d));c.appendChild(g),l.appendChild(c);let u=document.createElement("tbody");l.appendChild(u),e.appendChild(l);let p=f("div","placeholder","No tasks at info level or above.");e.appendChild(p);function x(){u.innerHTML="";let d=a.filter(({t:L})=>S(M(L))>=S(Ne));p.hidden=d.length>0,l.hidden=d.length===0;let b=L=>{let $=L.every(({t:N,i:C})=>i.get(C).size===N.artifacts.length);for(let{t:N,i:C}of L)i.set(C,new Set($?[]:N.artifacts.map(y=>y.kind)));x()},E,_=[],F=[];for(let[L,{t:$,i:N}]of d.entries()){let C=M($),y=f("tr","row");y.dataset.level=C;let D=f("td","level",Se[C]);D.title=C,y.appendChild(D);let v=$.from_sym??null,T=f("td","sym");if(v===null||v!==E){let m=L+1;for(;v!==null&&m<d.length&&d[m].t.from_sym===v;)m++;_=d.slice(L,m),F=[];let h=f("button","sym-btn",v??"\u2014");h.type="button",v===null&&h.classList.add(`${s}-sym-none`),T.appendChild(h)}E=v;let[le,q]=[_,F];q.push(T),T.title="Toggle all artifacts",T.addEventListener("click",()=>b(le));let P=m=>()=>q.forEach(h=>h.classList.toggle(`${s}-sym-hover`,m));T.addEventListener("mouseenter",P(!0)),T.addEventListener("mouseleave",P(!1)),y.appendChild(T),y.appendChild(f("td","kind",$.kind.replace(/^TASK_/,"")));let j=f("td","chips"),w=i.get(N);for(let m of $.artifacts){let h=f("button","chip",m.kind);h.type="button",h.setAttribute("aria-pressed",String(w.has(m.kind))),h.addEventListener("click",()=>{w.has(m.kind)?w.delete(m.kind):w.add(m.kind),x()}),j.appendChild(h)}y.appendChild(j);let J=f("td","id",Me($.id));if(J.title=$.id,y.appendChild(J),u.appendChild(y),w.size>0){let m=f("tr","detail"),h=document.createElement("td");h.colSpan=5;for(let z of $.artifacts)w.has(z.kind)&&h.appendChild(_e(z,()=>{w.delete(z.kind),x()}));m.appendChild(h),u.appendChild(m)}}}x()}var Ae=["task_entries","pre","post"],Ze={render({model:e,el:t}){let o=()=>{let n=e.get("task_entries");ne(t,{pre:e.get("pre"),post:e.get("post"),main:a=>ae(a,n??[]),hasMain:n!=null})};o();for(let n of Ae)e.on(`change:${n}`,o)}};export{Ze as default};
