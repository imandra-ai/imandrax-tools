type shape =
  | { tag: "Circle"; payload: number }
  | { tag: "Polygon"; payload: rect };