export type Option<T> = { tag: "Some"; payload: T } | null;

type a = unknown;
const w: Option<a> = null;