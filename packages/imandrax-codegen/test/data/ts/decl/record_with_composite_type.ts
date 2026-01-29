interface Some<T> {
	value: T;
}

export type Option<T> = Some<T> | null;

type shape = {
  circle: Option<number>;
};