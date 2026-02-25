#!/usr/bin/env janet

# Generate a tree view of skill/ markdown files with descriptions from frontmatter,
# and insert/update it in SKILL.md inside a ```tree {name: skill-dir-structure} code block.

# Resolve skill-dir relative to the script's own location, not cwd.
(def script-dir
  (let [exe (dyn *executable*)
        args (dyn *args*)
        script-path (if (and args (> (length args) 0)) (in args 0) exe)
        # find last /
        last-slash (last (string/find-all "/" script-path))]
    (if last-slash
      (string/slice script-path 0 last-slash)
      ".")))
(def skill-dir (string script-dir "/../skill"))
(def skill-md (string skill-dir "/SKILL.md"))

# ── Directory descriptions ──────────────────────────────────────────
# Map from relative dir path (under skill/) to its description.
# Must match the actual directory structure exactly.
(def dir-descriptions
  {"advanced"                                       "Advanced topics and tips"
   "examples"                                       "Worked examples"
   "examples/region-decomp"                         ""
   "examples/region-decomp/synthesized-real-world-examples" ""
   "reference"                                      "Language and API reference"
   "reference/api"                                  "Module-level API docs"})

# ── PEG grammars ────────────────────────────────────────────────────

(def frontmatter-peg
  "Match YAML frontmatter and capture the description value."
  (peg/compile
    ~{:main (* "---" :s* (some :field) "---")
      :field (+ :desc-field :other-field)
      :desc-field (* "description:" :s* (<- (to (+ "\n" -1))) :s*)
      :other-field (* (not "---") (thru "\n"))}))

(def codeblock-peg
  "Split content around the ```tree {name: skill-dir-structure} block.
   Captures: (before, after) — or no captures if block not found."
  (peg/compile
    ~(* (<- (to "```tree {name: skill-dir-structure}"))
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
  "Recursively collect .md files under dir, excluding SKILL.md."
  [dir]
  (def results @[])
  (defn walk [d]
    (each entry (sorted (os/dir d))
      (def full (string d "/" entry))
      (case (os/stat full :mode)
        :directory (walk full)
        :file (when (and (string/has-suffix? ".md" entry)
                         (not= entry "SKILL.md"))
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
    (def parts (string/split "/" rel))
    (def desc (parse-description f))
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

(def block (string "```tree {name: skill-dir-structure}\n"
                    "imandrax/\n"
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
(print block)
