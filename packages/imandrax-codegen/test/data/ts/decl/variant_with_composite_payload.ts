interface Some<T> {
	value: T;
}

export type Option<T> = Some<T> | null;

type shape2 =
  | { tag: "Circle"; payload: Option<number> };