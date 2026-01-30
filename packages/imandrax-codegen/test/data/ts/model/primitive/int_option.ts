export type Option<T> = { tag: "Some"; payload: T } | null;

const w: Option<number> = { tag: "Some", payload: 2 };