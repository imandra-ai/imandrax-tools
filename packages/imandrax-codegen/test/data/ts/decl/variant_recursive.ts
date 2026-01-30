type tree =
  | { tag: "Leaf"; payload: number }
  | { tag: "Node"; payload: [tree, tree] };