var g=/^\s*$/,v=/^(?:-(?:\s+|$))+/,H=/:[ \t]*(?:#.*)?$/,Y=/(?:^[ \t]*|[:-][ \t]+)[|>][+-]?\d{0,2}[ \t]*(?:#.*)?$/;function h(e){return/^ */.exec(e)[0].length}function b(e,n){return v.exec(e.slice(n))?.[0].length??0}function I(e,n){return n+Math.max(1,b(e,n))}function _(e,n){return b(e,n)===0&&H.test(e)?n:1/0}function A(e){return Y.test(e)}function E(e){return{text:e,indent:h(e),children:[],block:[]}}function k(e){let n=e.replace(/\n+$/,"").split(`
`),l=[],o=[],s=(r,c,i)=>{let u=o.length?o[o.length-1].node:null;(u?u.children:l).push(r),o.push({node:r,childIndent:c,seqIndent:i})};for(let r=0;r<n.length;r++){let c=n[r];if(g.test(c)){let a=E(c);o.length?o[o.length-1].node.children.push(a):l.push(a);continue}let i=h(c),u=b(c,i)>0;for(;o.length;){let a=o[o.length-1],f=u?Math.min(a.childIndent,a.seqIndent):a.childIndent;if(i>=f)break;o.pop()}let d=E(c);if(s(d,I(c,i),_(c,i)),!!A(c)){for(;r+1<n.length;){let a=n[r+1];if(!g.test(a)&&h(a)<=i)break;d.block.push(a),r++}for(;d.block.length&&g.test(d.block[d.block.length-1]);)d.block.pop(),r--;o.pop()}}return l}function x(e){let n=e.block.length;for(let l of e.children)n+=1+x(l);return n}var B=/^(?:-(?:[ \t]+|$))+/,O=/^("(?:[^"\\]|\\.)*"|'(?:[^']|'')*'|[^:#\s][^:]*?)(:)([ \t]|$)/,q=/^[|>][+-]?\d{0,2}$/,z=/^([&*]\S+|!!?\S*)([ \t]+|$)/,D=/^-?(?:\d[\d_]*(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$|^-?0[xXoObB][0-9a-fA-F_]+$|^[-+]?\.(?:inf|Inf|INF)$|^\.(?:nan|NaN|NAN)$/,K=/^(?:true|True|TRUE|false|False|FALSE|null|Null|NULL|~)$/,F=/^"(?:[^"\\]|\\.)*"|^'(?:[^']|'')*'/;function N(e){return e.replace(/[&<>]/g,n=>n==="&"?"&amp;":n==="<"?"&lt;":"&gt;")}function m(e,n){return`<span class="t-${e}">${N(n)}</span>`}function R(e){let n=/^[ \t]*/.exec(e)[0].length,l=F.exec(e.slice(n)),o=n+(l?l[0].length:0),s=/(?:^|[ \t])#/.exec(e.slice(o));if(!s)return[e,""];let r=o+s.index;return[e.slice(0,r),e.slice(r)]}function y(e){let[n,l]=R(e),o=/^[ \t]*/.exec(n)[0],s=n.slice(o.length),r=o,c=z.exec(s);if(c&&(r+=m("ref",c[1])+c[2],s=s.slice(c[0].length)),s){let i=q.test(s)?"block":K.test(s)?"lit":D.test(s)?"num":"str";r+=m(i,s)}return r+(l?m("comment",l):"")}function $(e){let n=/^[ \t]*/.exec(e)[0],l=e.slice(n.length),o=n;if(!l)return o;if(l==="---"||l==="...")return o+m("punct",l);let s=B.exec(l);if(s&&(o+=m("punct",s[0]),l=l.slice(s[0].length)),l.startsWith("#"))return o+m("comment",l);let r=O.exec(l);return r?(o+=m("key",r[1])+m("punct",":"),o+y(l.slice(r[1].length+1))):o+y(l)}function L(e){return N(e)}var t="imdx-jsonable",C=`
.${t} { font-family: ui-sans-serif, system-ui, sans-serif; font-size: 12px;
  color: #1a1d21; border: 1px solid #d8dde2; border-radius: 6px; overflow: hidden;
  background: #fff; box-sizing: border-box; }
.${t} *, .${t} *::before, .${t} *::after { box-sizing: border-box; }

.${t}-bar { display: flex; align-items: center; gap: 8px; padding: 6px 10px;
  background: #fafbfc; border-bottom: 1px solid #d8dde2; }
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
`;var J=3;function T(){let e=document.createElement("span");return e.className=`${t}-arrow`,e}function w(e){let n=document.createElement("span");return n.className=`${t}-text`,n.innerHTML=e,n}function U(e){let n=document.createElement("div");return n.className=`${t}-block`,n.innerHTML=e.map(L).join(`
`),n}function M(e,n){if(!(e.children.length>0||e.block.length>0)){let i=document.createElement("div");return i.className=`${t}-line`,i.append(T(),w($(e.text))),i}let o=document.createElement("details");o.className=`${t}-fold`,o.open=n<J;let s=document.createElement("summary");s.className=`${t}-line`,s.append(T(),w($(e.text)));let r=document.createElement("span");r.className=`${t}-count`;let c=x(e);r.textContent=`\u2026${c} line${c===1?"":"s"}`,s.appendChild(r),o.appendChild(s),e.block.length&&o.appendChild(U(e.block));for(let i of e.children)o.appendChild(M(i,n+1));return o}function S(e,n,l=""){e.innerHTML="",e.classList.add(t);let o=document.createElement("style");if(o.textContent=C,e.appendChild(o),!n||!n.trim()){let i=document.createElement("div");i.className=`${t}-placeholder`,i.textContent="Nothing to show.",e.appendChild(i);return}let s=k(n),r=document.createElement("div");r.className=`${t}-doc`;for(let i of s)r.appendChild(M(i,0));let c=document.createElement("div");c.className=`${t}-scroll`,c.appendChild(r),e.appendChild(j(r,n,l)),e.appendChild(c)}function j(e,n,l){let o=document.createElement("div");if(o.className=`${t}-bar`,l){let a=document.createElement("span");a.className=`${t}-label`,a.textContent=l,o.appendChild(a)}let s=document.createElement("span");s.className=`${t}-meta`;let r=n.replace(/\n+$/,"").split(`
`).length;s.textContent=`${r.toLocaleString()} line${r===1?"":"s"}`,o.appendChild(s);let c=document.createElement("div");c.className=`${t}-actions`;let i=(a,f)=>{let p=document.createElement("button");return p.className=`${t}-btn`,p.type="button",p.textContent=a,p.addEventListener("click",f),c.appendChild(p),p},u=a=>{for(let f of e.querySelectorAll("details"))f.open=a};i("expand all",()=>u(!0)),i("collapse all",()=>u(!1));let d=i("copy",()=>{navigator.clipboard?.writeText(n).then(()=>{d.textContent="copied",setTimeout(()=>d.textContent="copy",1200)})});return o.appendChild(c),o}var te={render({model:e,el:n}){let l=()=>S(n,e.get("yaml_str"),e.get("label"));l(),e.on("change:yaml_str",l),e.on("change:label",l)}};export{te as default};
