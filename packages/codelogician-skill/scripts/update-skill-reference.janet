#!/usr/bin/env janet

# Generate a tree view of markdown files with descriptions from frontmatter,
# and insert/update it in SKILL.md inside a ```tree {name: skill-dir-structure} code block.
#
# Usage: janet update-skill-reference.janet <template-dir> <output-skill-md>
# e.g.:  janet update-skill-reference.janet templates/skill skill/SKILL.md
#
# Scans <template-dir> for .md and .md.jinja files. Jinja files are shown
# with the .jinja extension stripped (their output name).

(def args (dyn *args*))
(unless (and args (>= (length args) 3))
  (eprintf "Usage: %s <template-dir> <output-skill-md>" (or (and args (in args 0)) "update-skill-reference.janet"))
  (os/exit 1))
(def skill-dir (in args 1))
(def skill-md (in args 2))
(def tree-block-identifier "skill-dir-structure")
(def root-name ".")

# ── Directory descriptions ──────────────────────────────────────────
# Map from relative dir path (under skill/) to its description.
# Must match the actual directory structure exactly.
(def dir-descriptions
  {"advanced"                                       "Advanced topics and tips"
   "examples"                                       "Worked examples"
   "examples/region-decomp"                         ""
   "examples/region-decomp/synthesized-real-world-examples" ""
   "reference"                                      "Language and API reference"
   "reference/prelude"                              "Module-level API docs"
   "extended-prelude"                               ""
   })

# ── PEG grammars ────────────────────────────────────────────────────

(def frontmatter-peg
  "Match YAML frontmatter and capture the description value."
  (peg/compile
    ~{:main (* "---" :s* (some :field) "---")
      :field (+ :desc-field :other-field)
      :desc-field (* "description:" :s* (<- (to (+ "\n" -1))) :s*)
      :other-field (* (not "---") (thru "\n"))}))

(def- codeblock-marker (string "```tree {name: " tree-block-identifier "}"))

(def codeblock-peg
  "Split content around the ```tree {name: ...} block.
   Captures: (before, after) — or no captures if block not found."
  (peg/compile
    ~(* (<- (to ,codeblock-marker))
        # skip opening fence line
        (thru "\n")
        # skip content lines until closing ```
        (any (* (not "```") (thru "\n")))
        "```"
        (<- (any 1)))))

# ── helpers ─────────────────────────────────────────────────────────

(defn parse-description
  "Extract description from YAML frontmatter of a markdown file."
  [filepath]
  (def content (slurp filepath))
  (if-let [caps (peg/match frontmatter-peg content)]
    (let [desc (string/trim (in caps 0))]
      (if (= desc "") nil desc))))

(defn collect-md-files
  "Recursively collect .md and .md.jinja files under dir."
  [dir]
  (def results @[])
  (defn walk [d]
    (each entry (sorted (os/dir d))
      (def full (string d "/" entry))
      (case (os/stat full :mode)
        :directory (walk full)
        :file (when (or (string/has-suffix? ".md" entry)
                        (string/has-suffix? ".md.jinja" entry))
                (array/push results full)))))
  (walk dir)
  results)

(defn collect-dirs
  "Recursively collect all subdirectory paths (relative to base-dir) under dir."
  [dir base-dir]
  (def results @[])
  (defn walk [d]
    (each entry (sorted (os/dir d))
      (def full (string d "/" entry))
      (when (= :directory (os/stat full :mode))
        (def rel (string/slice full (+ 1 (length base-dir))))
        (array/push results rel)
        (walk full))))
  (walk dir)
  results)

(defn validate-dir-descriptions
  "Validate that dir-descriptions keys match actual dirs. Error on mismatch."
  []
  (def actual-dirs (collect-dirs skill-dir skill-dir))
  (def actual-set (table))
  (each d actual-dirs (put actual-set d true))
  (def desc-set (table))
  (each k (keys dir-descriptions) (put desc-set k true))

  (def missing @[])
  (def redundant @[])
  (each d actual-dirs
    (unless (in desc-set d)
      (array/push missing d)))
  (each k (keys dir-descriptions)
    (unless (in actual-set k)
      (array/push redundant k)))

  (when (or (> (length missing) 0) (> (length redundant) 0))
    (def msgs @[])
    (when (> (length missing) 0)
      (array/push msgs (string "  Missing from dir-descriptions: " (string/join (sorted missing) ", "))))
    (when (> (length redundant) 0)
      (array/push msgs (string "  Redundant in dir-descriptions:  " (string/join (sorted redundant) ", "))))
    (eprintf "Error: dir-descriptions does not match actual directory structure.\n%s" (string/join msgs "\n"))
    (os/exit 1)))

(defn build-tree
  "Build a nested tree structure from file paths with their metadata."
  [files base-dir]
  (def root @{:children (table) :files @[]})
  (each f files
    (def rel (string/slice f (+ 1 (length base-dir))))
    (def rel (if (string/has-suffix? ".jinja" rel)
               (string/slice rel 0 (- (length rel) 6))
               rel))
    (def parts (string/split "/" rel))
    (def desc (if (= (last parts) "SKILL.md") nil (parse-description f)))
    (var node root)
    (for i 0 (- (length parts) 1)
      (def part (in parts i))
      (unless (in (node :children) part)
        (put (node :children) part @{:children (table) :files @[]}))
      (set node (in (node :children) part)))
    (array/push (node :files)
                {:filename (last parts)
                 :description desc}))
  root)

(defn render-tree
  "Render tree as lines with tree-drawing characters."
  [node &opt prefix dir-path]
  (default prefix "")
  (default dir-path "")
  (def lines @[])
  (def children-keys (sorted (keys (node :children))))
  (def files (node :files))
  (def total (+ (length children-keys) (length files)))
  (var idx 0)

  # render subdirectories first
  (each dir-name children-keys
    (++ idx)
    (def is-last (= idx total))
    (def connector (if is-last "└── " "├── "))
    (def child-prefix (if is-last (string prefix "    ") (string prefix "│   ")))
    (def child-path (if (= dir-path "") dir-name (string dir-path "/" dir-name)))
    (def dir-desc (get dir-descriptions child-path))
    (def label
      (if (and dir-desc (not= dir-desc ""))
        (string prefix connector dir-name "/ # " dir-desc)
        (string prefix connector dir-name "/")))
    (array/push lines label)
    (array/concat lines (render-tree (in (node :children) dir-name) child-prefix child-path)))

  # render files
  (each file files
    (++ idx)
    (def is-last (= idx total))
    (def connector (if is-last "└── " "├── "))
    (def fname (file :filename))
    (def desc (file :description))
    (if (and desc (not= desc ""))
      (array/push lines (string prefix connector fname " # " desc))
      (array/push lines (string prefix connector fname))))

  lines)

# ── main ────────────────────────────────────────────────────────────

(validate-dir-descriptions)

(def files (collect-md-files skill-dir))
(def tree (build-tree files skill-dir))
(def lines (render-tree tree))
(def tree-str (string/join lines "\n"))

(def block (string codeblock-marker "\n"
                    root-name "/\n"
                    tree-str "\n"
                    "```"))

# Read SKILL.md and insert/replace the block
(def content (slurp skill-md))

(def new-content
  (if-let [caps (peg/match codeblock-peg content)]
    (string (in caps 0) block (in caps 1))
    (string (string/trim content) "\n\n" block "\n")))

(spit skill-md new-content)
(print "Updated " skill-md)
# (print block)
