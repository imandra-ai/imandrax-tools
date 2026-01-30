export type Option<T> = { tag: "Some"; payload: T } | null;

const w: Option<a> = null;