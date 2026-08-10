var b=/^\s*$/,K=/^(?:-(?:\s+|$))+/,D=/:[ \t]*(?:#.*)?$/,F=/(?:^[ \t]*|[:-][ \t]+)[|>][+-]?\d{0,2}[ \t]*(?:#.*)?$/;function $(e){return/^ */.exec(e)[0].length}function y(e,t){return K.exec(e.slice(t))?.[0].length??0}function P(e,t){return t+Math.max(1,y(e,t))}function q(e,t){return y(e,t)===0&&D.test(e)?t:1/0}function j(e){return F.test(e)}function T(e){return{text:e,indent:$(e),children:[],block:[]}}function L(e){let t=e.replace(/\n+$/,"").split(`
`),o=[],n=[],c=(a,s,l)=>{let m=n.length?n[n.length-1].node:null;(m?m.children:o).push(a),n.push({node:a,childIndent:s,seqIndent:l})};for(let a=0;a<t.length;a++){let s=t[a];if(b.test(s)){let d=T(s);n.length?n[n.length-1].node.children.push(d):o.push(d);continue}let l=$(s),m=y(s,l)>0;for(;n.length;){let d=n[n.length-1],g=m?Math.min(d.childIndent,d.seqIndent):d.childIndent;if(l>=g)break;n.pop()}let p=T(s);if(c(p,P(s,l),q(s,l)),!!j(s)){for(;a+1<t.length;){let d=t[a+1];if(!b.test(d)&&$(d)<=l)break;p.block.push(d),a++}for(;p.block.length&&b.test(p.block[p.block.length-1]);)p.block.pop(),a--;n.pop()}}return o}function k(e){let t=e.block.length;for(let o of e.children)t+=1+k(o);return t}var J=/^(?:-(?:[ \t]+|$))+/,U=/^("(?:[^"\\]|\\.)*"|'(?:[^']|'')*'|[^:#\s][^:]*?)(:)([ \t]|$)/,Z=/^[|>][+-]?\d{0,2}$/,G=/^([&*]\S+|!!?\S*)([ \t]+|$)/,Q=/^-?(?:\d[\d_]*(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$|^-?0[xXoObB][0-9a-fA-F_]+$|^[-+]?\.(?:inf|Inf|INF)$|^\.(?:nan|NaN|NAN)$/,W=/^(?:true|True|TRUE|false|False|FALSE|null|Null|NULL|~)$/,X=/^"(?:[^"\\]|\\.)*"|^'(?:[^']|'')*'/;function w(e){return e.replace(/[&<>]/g,t=>t==="&"?"&amp;":t==="<"?"&lt;":"&gt;")}function u(e,t){return`<span class="t-${e}">${w(t)}</span>`}function V(e){let t=/^[ \t]*/.exec(e)[0].length,o=X.exec(e.slice(t)),n=t+(o?o[0].length:0),c=/(?:^|[ \t])#/.exec(e.slice(n));if(!c)return[e,""];let a=n+c.index;return[e.slice(0,a),e.slice(a)]}function S(e){let[t,o]=V(e),n=/^[ \t]*/.exec(t)[0],c=t.slice(n.length),a=n,s=G.exec(c);if(s&&(a+=u("ref",s[1])+s[2],c=c.slice(s[0].length)),c){let l=Z.test(c)?"block":W.test(c)?"lit":Q.test(c)?"num":"str";a+=u(l,c)}return a+(o?u("comment",o):"")}function E(e){let t=/^[ \t]*/.exec(e)[0],o=e.slice(t.length),n=t;if(!o)return n;if(o==="---"||o==="...")return n+u("punct",o);let c=J.exec(o);if(c&&(n+=u("punct",c[0]),o=o.slice(c[0].length)),o.startsWith("#"))return n+u("comment",o);let a=U.exec(o);return a?(n+=u("key",a[1])+u("punct",":"),n+S(o.slice(a[1].length+1))):n+S(o)}function M(e){return w(e)}var r="imdx-jsonable",v=`
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
`;var ee=3;function _(){let e=document.createElement("span");return e.className=`${r}-arrow`,e}function A(e){let t=document.createElement("span");return t.className=`${r}-text`,t.innerHTML=e,t}function te(e){let t=document.createElement("div");return t.className=`${r}-block`,t.innerHTML=e.map(M).join(`
`),t}function H(e,t){if(!(e.children.length>0||e.block.length>0)){let l=document.createElement("div");return l.className=`${r}-line`,l.append(_(),A(E(e.text))),l}let n=document.createElement("details");n.className=`${r}-fold`,n.open=t<ee;let c=document.createElement("summary");c.className=`${r}-line`,c.append(_(),A(E(e.text)));let a=document.createElement("span");a.className=`${r}-count`;let s=k(e);a.textContent=`\u2026${s} line${s===1?"":"s"}`,c.appendChild(a),n.appendChild(c),e.block.length&&n.appendChild(te(e.block));for(let l of e.children)n.appendChild(H(l,t+1));return n}function C(e,t,o=""){e.innerHTML="",e.classList.add(r);let n=document.createElement("style");if(n.textContent=v,e.appendChild(n),!t||!t.trim()){let l=document.createElement("div");l.className=`${r}-placeholder`,l.textContent="Nothing to show.",e.appendChild(l);return}let c=L(t),a=document.createElement("div");a.className=`${r}-doc`;for(let l of c)a.appendChild(H(l,0));let s=document.createElement("div");s.className=`${r}-scroll`,s.appendChild(a),e.appendChild(ne(a,t,o)),e.appendChild(s)}function ne(e,t,o){let n=document.createElement("div");if(n.className=`${r}-bar`,o){let d=document.createElement("span");d.className=`${r}-label`,d.textContent=o,n.appendChild(d)}let c=document.createElement("span");c.className=`${r}-meta`;let a=t.replace(/\n+$/,"").split(`
`).length;c.textContent=`${a.toLocaleString()} line${a===1?"":"s"}`,n.appendChild(c);let s=document.createElement("div");s.className=`${r}-actions`;let l=(d,g)=>{let f=document.createElement("button");return f.className=`${r}-btn`,f.type="button",f.textContent=d,f.addEventListener("click",g),s.appendChild(f),f},m=d=>{for(let g of e.querySelectorAll("details"))g.open=d};l("expand all",()=>m(!0)),l("collapse all",()=>m(!1));let p=l("copy",()=>{navigator.clipboard?.writeText(t).then(()=>{p.textContent="copied",setTimeout(()=>p.textContent="copy",1200)})});return n.appendChild(s),n}var h="imdx-stack",oe=`
.${h} { display: flex; flex-direction: column; gap: 8px; box-sizing: border-box; }
.${h}-placeholder { font-family: ui-sans-serif, system-ui, sans-serif;
  font-size: 12px; color: #9aa1a9; font-style: italic; padding: 10px;
  border: 1px solid #d8dde2; border-radius: 6px; background: #fff; }
`;function z(e,t){e.innerHTML="",e.classList.add(h);let o=document.createElement("style");o.textContent=oe,e.appendChild(o);let n=()=>{let s=document.createElement("div");return e.appendChild(s),s},c=!!(t.pre&&t.pre.trim()),a=!!(t.post&&t.post.trim());if(c&&C(n(),t.pre),t.hasMain&&t.main(n()),a&&C(n(),t.post),!c&&!a&&!t.hasMain){let s=document.createElement("div");s.className=`${h}-placeholder`,s.textContent="Nothing to show.",e.appendChild(s)}}var O=new RegExp([/(?<str>'''[\s\S]*?'''|"""[\s\S]*?"""|'(?:[^'\\]|\\.)*'|"(?:[^"\\]|\\.)*")/,/(?<lit>\b(?:None|True|False)\b)/,/(?<cls>[A-Za-z_]\w*(?=\())/,/(?<attr>[A-Za-z_]\w*(?=\s*=))/,/(?<ident>[A-Za-z_]\w*)/,/(?<num>-?\d+(?:\.\d+)?)/].map(e=>e.source).join("|"),"g"),Y={str:"t-str",lit:"t-lit",cls:"t-cls",attr:"t-attr",num:"t-num"};function x(e){return e.replace(/[&<>]/g,t=>t==="&"?"&amp;":t==="<"?"&lt;":"&gt;")}function I(e){let t="",o=0;for(let n=O.exec(e);n;n=O.exec(e)){t+=x(e.slice(o,n.index));let c=n.groups??{},a=Object.keys(Y).find(s=>c[s]!==void 0);t+=a?`<span class="${Y[a]}">${x(n[0])}</span>`:x(n[0]),o=n.index+n[0].length}return t+=x(e.slice(o)),t}var i="imdx-task",B=`
.${i} { display: flex; flex-direction: column; gap: 8px;
  font-family: ui-sans-serif, system-ui, sans-serif; font-size: 12px;
  color: #1a1d21; box-sizing: border-box; }
.${i} *, .${i} *::before, .${i} *::after { box-sizing: border-box; }

.${i}-task, .${i}-art { border: 1px solid #d8dde2; border-radius: 6px;
  overflow: hidden; }
.${i}-task { background: #fafbfc; }

.${i}-summary { display: flex; align-items: center; gap: 8px; padding: 6px 10px;
  cursor: pointer; user-select: none; list-style: none; }
.${i}-summary::-webkit-details-marker { display: none; }
.${i}-summary::before { content: "\\25B8"; color: #6b727b; font-size: 10px;
  transition: transform 0.12s ease; }
details[open] > .${i}-summary::before { transform: rotate(90deg); }

.${i}-kind { font-weight: 600; letter-spacing: 0.02em; }
.${i}-id { color: #6b727b; font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 11px; }
.${i}-meta { margin-left: auto; color: #6b727b; font-size: 11px;
  font-variant-numeric: tabular-nums; }

.${i}-body { padding: 8px; display: flex; flex-direction: column; gap: 8px; }
.${i}-art { background: #fff; }
.${i}-art-icon { font-size: 12px; line-height: 1; }
.${i}-art-kind { font-weight: 600; color: #1a1d21;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }

.${i}-copy { margin-left: auto; font: inherit; font-size: 11px; color: #6b727b;
  background: transparent; border: 1px solid #d8dde2; border-radius: 4px; padding: 1px 6px;
  cursor: pointer; }
.${i}-copy:hover { color: #1a1d21; border-color: #b7c0c9; }

.${i}-scroll { max-height: 720px; overflow: auto; border-top: 1px solid #d8dde2; }
.${i}-pre { margin: 0; padding: 10px; white-space: pre; tab-size: 2; font-size: 12px;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }

/* Syntax highlighting for the Python-repr artifact text (see task/highlight.ts).
   Light palette tuned for the #fff code bg. */
.${i}-pre .t-cls { color: #8250df; }   /* constructor / class names */
.${i}-pre .t-attr { color: #0550ae; }  /* keyword-arg names */
.${i}-pre .t-str { color: #0a7d33; }   /* string literals */
.${i}-pre .t-num { color: #953800; }   /* numbers */
.${i}-pre .t-lit { color: #cf222e; }   /* None / True / False */

.${i}-placeholder { color: #9aa1a9; font-style: italic; padding: 8px; }
`;var N={success:"\u2705",error:"\u274C",warning:"\u26A0\uFE0F",info:"\u2139\uFE0F",in_progress:"\u{1F6A7}",pending:"\u23F3",running:"\u23F1\uFE0F",skipped:"\u23ED\uFE0F",unknown:"\u2753",healthy:"\u{1F7E2}",degraded:"\u{1F7E1}",down:"\u{1F534}"};function re(e,t){if(e==="po_res")return t.includes("res=POSuccessProof")?N.success:t.includes("res=POErrorProof")?N.warning:N.error}function se(e){let t=document.createElement("details");t.className=`${i}-art`,t.open=!0;let o=document.createElement("summary");o.className=`${i}-summary`;let n=document.createElement("span");n.className=`${i}-art-kind`,n.textContent=e.kind,o.appendChild(n);let c=re(e.kind,e.repr);if(c){let p=document.createElement("span");p.className=`${i}-art-icon`,p.textContent=c,o.appendChild(p)}let a=document.createElement("span");a.className=`${i}-meta`,a.textContent=`${e.repr.length.toLocaleString()} chars`,o.appendChild(a);let s=document.createElement("button");s.className=`${i}-copy`,s.type="button",s.textContent="copy",s.addEventListener("click",p=>{p.preventDefault(),p.stopPropagation(),navigator.clipboard?.writeText(e.repr).then(()=>{s.textContent="copied",setTimeout(()=>s.textContent="copy",1200)})}),o.appendChild(s),t.appendChild(o);let l=document.createElement("div");l.className=`${i}-scroll`;let m=document.createElement("pre");return m.className=`${i}-pre`,m.innerHTML=I(e.repr),l.appendChild(m),t.appendChild(l),t}function ae(e){let t=document.createElement("details");t.className=`${i}-task`,t.open=!0;let o=document.createElement("summary");o.className=`${i}-summary`;let n=document.createElement("span");if(n.className=`${i}-kind`,n.textContent=e.kind,o.appendChild(n),e.id){let l=document.createElement("span");l.className=`${i}-id`,l.textContent=e.id,o.appendChild(l)}let c=document.createElement("span");c.className=`${i}-meta`;let a=e.artifacts.length;c.textContent=`${a} artifact${a===1?"":"s"}`,o.appendChild(c),t.appendChild(o);let s=document.createElement("div");s.className=`${i}-body`;for(let l of e.artifacts)s.appendChild(se(l));return t.appendChild(s),t}function R(e,t){e.innerHTML="",e.classList.add(i);let o=document.createElement("style");if(o.textContent=B,e.appendChild(o),!t||t.length===0){let n=document.createElement("div");n.className=`${i}-placeholder`,n.textContent="No tasks.",e.appendChild(n);return}for(let n of t)e.appendChild(ae(n))}var ie=["task_entries","pre","post"],Ne={render({model:e,el:t}){let o=()=>{let n=e.get("task_entries");z(t,{pre:e.get("pre"),post:e.get("post"),main:c=>R(c,n??[]),hasMain:n!=null})};o();for(let n of ie)e.on(`change:${n}`,o)}};export{Ne as default};
