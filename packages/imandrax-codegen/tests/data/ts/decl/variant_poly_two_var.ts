type container<a, b> =
  | { tag: "Empty"; payload: null }
  | { tag: "Single"; payload: a }
  | { tag: "Pair"; payload: [a, b] }
  | { tag: "Labeled"; payload: { key: a; value: b } }
  | { tag: "Multi"; payload: [a[], b[]] };