export class DefaultMap<K, V extends NonNullable<unknown>>
	implements Map<K, V>
{
	private map: Map<K, V>;

	constructor(
		private readonly defaultFactory: () => V,
		entries?: Iterable<[K, V]>,
	) {
		this.map = new Map(entries);
	}

	get size(): number {
		return this.map.size;
	}

	get(key: K): V {
		if (this.map.has(key)) {
			const value = this.map.get(key);
			if (value === undefined) {
				throw new Error("DefaultMap contains undefined value");
			}
			return value;
		}
		const defaultV = this.defaultFactory();
		this.map.set(key, defaultV);
		return defaultV;
	}

	set(key: K, value: V): this {
		this.map.set(key, value);
		return this;
	}

	has(key: K): boolean {
		return this.map.has(key);
	}

	delete(key: K): boolean {
		return this.map.delete(key);
	}

	clear(): void {
		this.map.clear();
	}

	forEach(
		callbackfn: (value: V, key: K, map: Map<K, V>) => void,
		thisArg?: unknown,
	): void {
		this.map.forEach(callbackfn, thisArg);
	}

	keys(): IterableIterator<K> {
		return this.map.keys();
	}

	values(): IterableIterator<V> {
		return this.map.values();
	}

	entries(): IterableIterator<[K, V]> {
		return this.map.entries();
	}

	[Symbol.iterator](): IterableIterator<[K, V]> {
		return this.map[Symbol.iterator]();
	}

	get [Symbol.toStringTag](): string {
		return "DefaultMap";
	}
}

const w: DefaultMap<number, boolean> = new DefaultMap((() => false), [[1, true], [3, true], [2, true]]);