type shape_poly<a> =
  | { tag: "Point"; payload: null }
  | { tag: "Circle"; payload: a }
  | { tag: "Rectangle"; payload: [a, a] }
  | { tag: "Triangle"; payload: { a: a; b: a; c: a } };