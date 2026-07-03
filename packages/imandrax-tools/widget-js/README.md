# imandrax-api-widget

The [anywidget](https://anywidget.dev/) front ends for rendering ImandraX results
in web. Jupyter notebooks are currently the first-class supported environment. 

Each widget is a TypeScript bundle that the Python package (`imandrax_tools.widget`) 
attaches to a result type's `_repr_mimebundle_`.

Widgets:
- `TasksWidget`: `EvalRes`, `CodeSnippetEvalResult`
- `RegionDecompWidget`: `EnrichedDecomposeRes` / `DecomposeRes`

## Development

## Generated types

The shape each widget consumes is defined by pydantic models on the Python side
(the single source of truth). A [script](./scripts/gen_py2ts_types.py) runs
[`pydantic-to-typescript`](https://github.com/phillipdupuis/pydantic-to-typescript)
over those models and emits [TS type definitions](./src/generated/node.ts). 

Each widget's handwritten `types.ts` re-exports the interfaces it needs from there.

## Bundle

`npm run build` bundles the TypeScript source into a single JavaScript bundle in 
the [corresponding Python package directory](../src/imandrax_tools/widget/static).
