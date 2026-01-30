export type Option<T> = { tag: "Some"; payload: T } | null;

type my_ty = {
  x: number;
  y: Option<number>;
  z: number;
};