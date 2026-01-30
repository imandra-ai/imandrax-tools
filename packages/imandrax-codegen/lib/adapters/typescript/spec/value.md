## Map int to bool with no element

```iml
let v : (int, bool) Map.t =
  Map.const false
```

```ts
const w = new DefaultMap<number, boolean>(() => false);
```


## Map int to bool

```iml
let v : (int, bool) Map.t =
  Map.const false
    |> Map.add 2 true
    |> Map.add 3 false
```


```ts
const w = new DefaultMap<number, boolean>(() => false, [[2, true]]);
```
