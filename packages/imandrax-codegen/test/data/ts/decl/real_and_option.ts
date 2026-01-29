interface Some<T> {
	value: T;
}

export type Option<T> = Some<T> | null;

type my_ty = {
  x: number;
  y: Option<number>;
  z: number;
};