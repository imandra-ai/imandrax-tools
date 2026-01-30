interface Some<T> {
	value: T;
}

export type Option<T> = Some<T> | null;

const w: Option<a> = null;