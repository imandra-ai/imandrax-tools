type shape =
  | { tag: "Point"; payload: null }
  | { tag: "Circle"; payload: number }
  | { tag: "Rectangle"; payload: [number, number] }
  | { tag: "Triangle"; payload: { a: number; b: number; c: number } };