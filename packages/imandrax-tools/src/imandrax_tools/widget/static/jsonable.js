var b=/^\s*$/,A=/^(?:-(?:\s+|$))+/,_=/:[ \t]*(?:#.*)?$/,B=/(?:^[ \t]*|[:-][ \t]+)[|>][+-]?\d{0,2}[ \t]*(?:#.*)?$/;function x(e){return/^ */.exec(e)[0].length}function $(e,n){return A.exec(e.slice(n))?.[0].length??0}function O(e,n){return n+Math.max(1,$(e,n))}function q(e,n){return $(e,n)===0&&_.test(e)?n:1/0}function z(e){return B.test(e)}function N(e){return{text:e,indent:x(e),children:[],block:[]}}function L(e){let n=e.replace(/\n+$/,"").split(`
`),r=[],o=[],l=(s,i,a)=>{let p=o.length?o[o.length-1].node:null;(p?p.children:r).push(s),o.push({node:s,childIndent:i,seqIndent:a})};for(let s=0;s<n.length;s++){let i=n[s];if(b.test(i)){let d=N(i);o.length?o[o.length-1].node.children.push(d):r.push(d);continue}let a=x(i),p=$(i,a)>0;for(;o.length;){let d=o[o.length-1],g=p?Math.min(d.childIndent,d.seqIndent):d.childIndent;if(a>=g)break;o.pop()}let m=N(i);if(l(m,O(i,a),q(i,a)),!!z(i)){for(;s+1<n.length;){let d=n[s+1];if(!b.test(d)&&x(d)<=a)break;m.block.push(d),s++}for(;m.block.length&&b.test(m.block[m.block.length-1]);)m.block.pop(),s--;o.pop()}}return r}function E(e){let n=e.block.length;for(let r of e.children)n+=1+E(r);return n}var D=/^(?:-(?:[ \t]+|$))+/,K=/^("(?:[^"\\]|\\.)*"|'(?:[^']|'')*'|[^:#\s][^:]*?)(:)([ \t]|$)/,F=/^[|>][+-]?\d{0,2}$/,R=/^([&*]\S+|!!?\S*)([ \t]+|$)/,J=/^-?(?:\d[\d_]*(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$|^-?0[xXoObB][0-9a-fA-F_]+$|^[-+]?\.(?:inf|Inf|INF)$|^\.(?:nan|NaN|NAN)$/,P=/^(?:true|True|TRUE|false|False|FALSE|null|Null|NULL|~)$/,U=/^"(?:[^"\\]|\\.)*"|^'(?:[^']|'')*'/;function T(e){return e.replace(/[&<>]/g,n=>n==="&"?"&amp;":n==="<"?"&lt;":"&gt;")}function u(e,n){return`<span class="t-${e}">${T(n)}</span>`}function j(e){let n=/^[ \t]*/.exec(e)[0].length,r=U.exec(e.slice(n)),o=n+(r?r[0].length:0),l=/(?:^|[ \t])#/.exec(e.slice(o));if(!l)return[e,""];let s=o+l.index;return[e.slice(0,s),e.slice(s)]}function C(e){let[n,r]=j(e),o=/^[ \t]*/.exec(n)[0],l=n.slice(o.length),s=o,i=R.exec(l);if(i&&(s+=u("ref",i[1])+i[2],l=l.slice(i[0].length)),l){let a=F.test(l)?"block":P.test(l)?"lit":J.test(l)?"num":"str";s+=u(a,l)}return s+(r?u("comment",r):"")}function k(e){let n=/^[ \t]*/.exec(e)[0],r=e.slice(n.length),o=n;if(!r)return o;if(r==="---"||r==="...")return o+u("punct",r);let l=D.exec(r);if(l&&(o+=u("punct",l[0]),r=r.slice(l[0].length)),r.startsWith("#"))return o+u("comment",r);let s=K.exec(r);return s?(o+=u("key",s[1])+u("punct",":"),o+C(r.slice(s[1].length+1))):o+C(r)}function v(e){return T(e)}var t="imdx-jsonable",w=`
.${t} { font-family: ui-sans-serif, system-ui, sans-serif; font-size: 12px;
  color: #1a1d21; border: 1px solid #d8dde2; border-radius: 6px; overflow: hidden;
  background: #fff; box-sizing: border-box; }
.${t} *, .${t} *::before, .${t} *::after { box-sizing: border-box; }

.${t}-bar { display: flex; align-items: center; gap: 8px; padding: 6px 10px;
  background: #fafbfc; border-bottom: 1px solid #d8dde2; }
.${t}-bar { cursor: pointer; user-select: none; }
.${t}-bar:hover { background: #f6f8fa; }
.${t}-bar:focus-visible { outline: 2px solid #b7c0c9; outline-offset: -2px; }
.${t}-collapsed .${t}-bar { border-bottom: 0; }
.${t}-collapsed .${t}-scroll { display: none; }
.${t}-label { font-weight: 600; letter-spacing: 0.02em; }
.${t}-meta { color: #6b727b; font-size: 11px; font-variant-numeric: tabular-nums; }
.${t}-actions { margin-left: auto; display: flex; gap: 6px; }
.${t}-btn { font: inherit; font-size: 11px; color: #6b727b; background: transparent;
  border: 1px solid #d8dde2; border-radius: 4px; padding: 1px 6px; cursor: pointer; }
.${t}-btn:hover { color: #1a1d21; border-color: #b7c0c9; }

.${t}-scroll { max-height: 720px; overflow: auto; padding: 8px 0; }
.${t}-doc { font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 12px; line-height: 1.5; tab-size: 2; }

.${t}-line { display: flex; align-items: baseline; padding: 0 10px 0 4px; }
.${t}-line:hover { background: #f4f6f8; }
summary.${t}-line { cursor: pointer; user-select: none; list-style: none; }
summary.${t}-line::-webkit-details-marker { display: none; }

/* The fold gutter: same width on foldable and leaf lines, so text stays aligned. */
.${t}-arrow { flex: 0 0 1.1em; color: #9aa1a9; font-size: 9px; line-height: 1.7;
  text-align: center; }
summary.${t}-line > .${t}-arrow::before { content: "\\25B8"; display: inline-block;
  transition: transform 0.12s ease; }
details[open] > summary.${t}-line > .${t}-arrow::before { transform: rotate(90deg); }
summary.${t}-line:hover > .${t}-arrow { color: #1a1d21; }

.${t}-text { white-space: pre; }
.${t}-count { margin-left: 10px; color: #9aa1a9; font-size: 11px; font-style: italic;
  font-variant-numeric: tabular-nums; }
details[open] > summary > .${t}-count { display: none; }

/* Block-scalar bodies (\`key: |\`) \u2014 opaque text, dimmed and rendered verbatim. */
.${t}-block { margin: 0; padding: 0 10px 0 calc(1.1em + 4px); white-space: pre;
  color: #3c4249; }

/* Token colors (see jsonable/highlight.ts); light palette tuned for the #fff bg. */
.${t}-text .t-key { color: #0550ae; }      /* mapping keys */
.${t}-text .t-str { color: #0a7d33; }      /* quoted and plain scalars */
.${t}-text .t-num { color: #953800; }      /* numbers */
.${t}-text .t-lit { color: #cf222e; }      /* true / false / null / ~ */
.${t}-text .t-punct { color: #6b727b; }    /* \`-\`, \`:\`, \`---\` */
.${t}-text .t-ref { color: #8250df; }      /* anchors / aliases / tags */
.${t}-text .t-block { color: #8250df; }    /* \`|\` / \`>\` indicators */
.${t}-text .t-comment { color: #9aa1a9; font-style: italic; }

.${t}-placeholder { color: #9aa1a9; font-style: italic; padding: 10px; }
`;var G=3;function S(){let e=document.createElement("span");return e.className=`${t}-arrow`,e}function M(e){let n=document.createElement("span");return n.className=`${t}-text`,n.innerHTML=e,n}function Q(e){let n=document.createElement("div");return n.className=`${t}-block`,n.innerHTML=e.map(v).join(`
`),n}function H(e,n){if(!(e.children.length>0||e.block.length>0)){let a=document.createElement("div");return a.className=`${t}-line`,a.append(S(),M(k(e.text))),a}let o=document.createElement("details");o.className=`${t}-fold`,o.open=n<G;let l=document.createElement("summary");l.className=`${t}-line`,l.append(S(),M(k(e.text)));let s=document.createElement("span");s.className=`${t}-count`;let i=E(e);s.textContent=`\u2026${i} line${i===1?"":"s"}`,l.appendChild(s),o.appendChild(l),e.block.length&&o.appendChild(Q(e.block));for(let a of e.children)o.appendChild(H(a,n+1));return o}function Y(e,n,r=""){e.innerHTML="",e.classList.add(t);let o=document.createElement("style");if(o.textContent=w,e.appendChild(o),!n||!n.trim()){let a=document.createElement("div");a.className=`${t}-placeholder`,a.textContent="Nothing to show.",e.appendChild(a);return}let l=L(n),s=document.createElement("div");s.className=`${t}-doc`;for(let a of l)s.appendChild(H(a,0));let i=document.createElement("div");i.className=`${t}-scroll`,i.appendChild(s),e.appendChild(W(e,s,n,r)),e.appendChild(i)}function W(e,n,r,o){let l=document.createElement("div");l.className=`${t}-bar`,l.tabIndex=0;let s=c=>{e.classList.toggle(`${t}-collapsed`,c),l.setAttribute("aria-expanded",String(!c)),l.title=c?"Expand":"Collapse"},i=()=>s(!e.classList.contains(`${t}-collapsed`));if(s(!1),l.addEventListener("click",i),l.addEventListener("keydown",c=>{c.target!==l||c.key!=="Enter"&&c.key!==" "||(c.preventDefault(),i())}),o){let c=document.createElement("span");c.className=`${t}-label`,c.textContent=o,l.appendChild(c)}let a=document.createElement("span");a.className=`${t}-meta`;let p=r.replace(/\n+$/,"").split(`
`).length;a.textContent=`${p.toLocaleString()} line${p===1?"":"s"}`,l.appendChild(a);let m=document.createElement("div");m.className=`${t}-actions`;let d=(c,h)=>{let f=document.createElement("button");return f.className=`${t}-btn`,f.type="button",f.textContent=c,f.addEventListener("click",I=>{I.stopPropagation(),h()}),m.appendChild(f),f},g=c=>{for(let h of n.querySelectorAll("details"))h.open=c};n.querySelector("details")&&(d("unfold all",()=>g(!0)),d("fold all",()=>g(!1)));let y=d("copy",()=>{navigator.clipboard?.writeText(r).then(()=>{y.textContent="copied",setTimeout(()=>y.textContent="copy",1200)})});return l.appendChild(m),l}var re={render({model:e,el:n}){let r=()=>Y(n,e.get("yaml_str"),e.get("label"));r(),e.on("change:yaml_str",r),e.on("change:label",r)}};export{re as default};
