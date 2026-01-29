interface Some<T> {
	value: T;
}

type Option<T> = Some<T> | null;
