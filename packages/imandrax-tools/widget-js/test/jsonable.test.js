import { describe, expect, it } from "vitest";

import { foldYaml, hiddenLineCount } from "../src/jsonable/fold";
import { highlightLine } from "../src/jsonable/highlight";
import { drawJsonable } from "../src/jsonable/view";

// The widget input is a single YAML string produced by `yaml_utils.to_yaml_str`
// on the Python side, so the fixtures here are literal YAML rather than
// generated JSON: block style, insertion-ordered keys, literal blocks for
// multi-line strings, and — PyYAML's default — sequence items at the *same*
// indentation as the key they belong to.
const DOC = `region_groups:
- id: 3
  status: Unknown
  constraints:
  - x > 0
  - y <= 10
- id: 4
  status: Verified
errors: []
`;

// The same document written with sequences indented under their key, which other
// emitters (and hand-written YAML) produce.
const INDENTED_SEQ = `region_groups:
  - id: 3
    constraints:
      - x > 0
  - id: 4
errors: []
`;

const WITH_BLOCK = `po_res:
  proof: |
    goal:
      x > 0
    qed
  count: 2
`;

describe("jsonable/fold", () => {
  it("nests lines by indentation", () => {
    const [groups, errors] = foldYaml(DOC);
    expect(groups.text).toBe("region_groups:");
    expect(groups.children.length).toBe(2); // two sequence items
    expect(errors.text).toBe("errors: []");
    expect(errors.children).toEqual([]);
  });

  it("nests a sequence written at its key's own indentation", () => {
    // PyYAML's default: `- id: 3` sits in column 0 yet belongs to `region_groups:`,
    // while `errors:` -- not a sequence item -- is a sibling of that key.
    const roots = foldYaml(DOC);
    expect(roots.map((n) => n.text)).toEqual(["region_groups:", "errors: []"]);
    expect(roots[0].children.map((n) => n.text)).toEqual([
      "- id: 3",
      "- id: 4",
    ]);
  });

  it("nests a sequence written indented under its key", () => {
    const roots = foldYaml(INDENTED_SEQ);
    expect(roots.map((n) => n.text)).toEqual(["region_groups:", "errors: []"]);
    expect(roots[0].children.map((n) => n.text)).toEqual([
      "  - id: 3",
      "  - id: 4",
    ]);
  });

  it("hangs a sequence item's keys off the item, not the dash column", () => {
    const [first] = foldYaml(DOC)[0].children;
    expect(first.text).toBe("- id: 3");
    // `status:`/`constraints:` sit at the item's content column, so they nest
    // under it rather than under `region_groups:`.
    expect(first.children.map((c) => c.text.trim())).toEqual([
      "status: Unknown",
      "constraints:",
    ]);
    expect(first.children[1].children.map((c) => c.text.trim())).toEqual([
      "- x > 0",
      "- y <= 10",
    ]);
  });

  it("captures a block scalar's body verbatim, unparsed", () => {
    const proof = foldYaml(WITH_BLOCK)[0].children[0];
    expect(proof.text).toBe("  proof: |");
    expect(proof.block).toEqual(["    goal:", "      x > 0", "    qed"]);
    // The body's own indentation does not create fold nodes...
    expect(proof.children).toEqual([]);
    // ...and the key that follows the block is a sibling of `proof:`.
    expect(foldYaml(WITH_BLOCK)[0].children[1].text).toBe("  count: 2");
  });

  it("counts the lines a fold hides", () => {
    expect(hiddenLineCount(foldYaml(DOC)[0])).toBe(7); // 2 items + 5 nested lines
    expect(hiddenLineCount(foldYaml(WITH_BLOCK)[0])).toBe(5); // 2 keys + 3 block lines
  });

  it("preserves every line of the input", () => {
    const flat = (n) => [n.text, ...n.block, ...n.children.flatMap(flat)];
    for (const doc of [DOC, INDENTED_SEQ, WITH_BLOCK]) {
      expect(foldYaml(doc).flatMap(flat).join("\n")).toBe(doc.trimEnd());
    }
  });
});

describe("jsonable/highlight", () => {
  // Token text is what matters; the CSS classes are asserted by kind.
  const tokens = (line) => {
    const el = document.createElement("div");
    el.innerHTML = highlightLine(line);
    return [...el.querySelectorAll("span")].map((s) => [
      s.className,
      s.textContent,
    ]);
  };

  it("marks keys, punctuation, and scalars", () => {
    expect(tokens("  status: Unknown")).toEqual([
      ["t-key", "status"],
      ["t-punct", ":"],
      ["t-str", "Unknown"],
    ]);
  });

  it("distinguishes numbers and literals from strings", () => {
    expect(tokens("count: 42").at(-1)).toEqual(["t-num", "42"]);
    expect(tokens("count: -1.5e3").at(-1)).toEqual(["t-num", "-1.5e3"]);
    expect(tokens("res: null").at(-1)).toEqual(["t-lit", "null"]);
    expect(tokens("ok: true").at(-1)).toEqual(["t-lit", "true"]);
    expect(tokens("name: 'len_append'").at(-1)).toEqual([
      "t-str",
      "'len_append'",
    ]);
  });

  it("marks sequence dashes and block indicators", () => {
    expect(tokens("  - x > 0")[0][0]).toBe("t-punct");
    expect(tokens("  proof: |").at(-1)).toEqual(["t-block", "|"]);
    expect(tokens("  proof: |-").at(-1)).toEqual(["t-block", "|-"]);
  });

  it("treats a trailing # as a comment but not one inside quotes", () => {
    expect(tokens("x: 1 # note").at(-1)).toEqual(["t-comment", " # note"]);
    expect(tokens("msg: 'a # b'").at(-1)).toEqual(["t-str", "'a # b'"]);
  });

  it("leaves the line's text byte-for-byte intact", () => {
    for (const line of DOC.trimEnd().split("\n")) {
      const el = document.createElement("div");
      el.innerHTML = highlightLine(line);
      expect(el.textContent).toBe(line);
    }
  });

  it("escapes HTML in the source text", () => {
    const el = document.createElement("div");
    el.innerHTML = highlightLine("expr: <a> & </b>");
    expect(el.textContent).toBe("expr: <a> & </b>");
    expect(el.querySelector("a")).toBeNull();
  });
});

describe("jsonable/view", () => {
  const render = (yaml, label) => {
    const el = document.createElement("div");
    drawJsonable(el, yaml, label);
    return el;
  };

  it("renders the document verbatim", () => {
    const doc = render(DOC).querySelector(".imdx-jsonable-doc");
    expect(doc.textContent).toContain("region_groups:");
    // Collapsed-fold hints are the only added text, so strip them out.
    for (const c of doc.querySelectorAll(".imdx-jsonable-count")) c.remove();
    expect(doc.textContent).toBe(DOC.trimEnd().split("\n").join(""));
  });

  it("makes lines with nested content foldable and leaves leaves alone", () => {
    const el = render(DOC);
    const folds = [...el.querySelectorAll(".imdx-jsonable-fold")];
    const summaries = folds.map((f) => f.querySelector("summary").textContent);
    expect(summaries[0]).toContain("region_groups:");
    // `errors: []` has nothing nested under it -> a plain line, not a <details>.
    expect(summaries.some((s) => s.includes("errors:"))).toBe(false);
  });

  it("labels a collapsed fold with the number of hidden lines", () => {
    const el = render(DOC);
    const count = el.querySelector(".imdx-jsonable-count");
    expect(count.textContent).toBe("…7 lines");
  });

  it("opens the outer levels and collapses deeper ones", () => {
    // One fold per level: a: / b: / c: / d:, at depths 0..3.
    const deep = "a:\n  b:\n    c:\n      d:\n        e: 1\n";
    const open = [...render(deep).querySelectorAll(".imdx-jsonable-fold")].map(
      (f) => f.open,
    );
    expect(open).toEqual([true, true, true, false]);
  });

  it("expands and collapses everything from the toolbar", () => {
    const el = render(DOC);
    const btn = (text) =>
      [...el.querySelectorAll(".imdx-jsonable-btn")].find(
        (b) => b.textContent === text,
      );
    const folds = [...el.querySelectorAll(".imdx-jsonable-fold")];

    btn("expand all").click();
    expect(folds.every((f) => f.open)).toBe(true);
    btn("collapse all").click();
    expect(folds.some((f) => f.open)).toBe(false);
  });

  it("shows the label and line count in the toolbar", () => {
    const el = render(DOC, "verify result");
    expect(el.querySelector(".imdx-jsonable-label").textContent).toBe(
      "verify result",
    );
    expect(el.querySelector(".imdx-jsonable-meta").textContent).toBe("9 lines");
  });

  it("renders a block scalar's body as one unhighlighted chunk", () => {
    const block = render(WITH_BLOCK).querySelector(".imdx-jsonable-block");
    expect(block.textContent).toBe("    goal:\n      x > 0\n    qed");
    expect(block.querySelector("span")).toBeNull();
  });

  it("tolerates empty input", () => {
    for (const empty of ["", "\n", "   "]) {
      const el = render(empty);
      expect(el.querySelector(".imdx-jsonable-placeholder").textContent).toBe(
        "Nothing to show.",
      );
    }
  });
});
