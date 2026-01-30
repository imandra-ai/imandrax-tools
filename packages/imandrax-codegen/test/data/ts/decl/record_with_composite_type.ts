export type Option<T> = { tag: "Some"; payload: T } | null;

type shape = {
  circle: Option<number>;
};