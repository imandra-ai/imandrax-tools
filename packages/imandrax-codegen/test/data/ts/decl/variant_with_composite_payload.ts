export type Option<T> = { tag: "Some"; payload: T } | null;

type shape2 =
  | { tag: "Circle"; payload: Option<number> };