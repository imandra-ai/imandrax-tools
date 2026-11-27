---
name: client-libraries
description: Libraries for programmatically interacting with ImandraX - the Python and TypeScript clients (with a quickstart covering connecting, the main endpoints, reading results and the MIR db), plus Python utilities for protobuf models, pretty-printing and IML code manipulation.
tags: [reference]
---

# Client libraries

Source code: GitHub `imandra-ai/imandrax-api/src/{py,ts}/`

- Python: `imandrax-api` (PyPI). Use the v2 client, `imandrax_api.v2`.
- TypeScript: `@imandra/imandrax-api` (npm).

## Quickstart

The two clients have the same API: `snake_case` in Python, `camelCase` in TypeScript.

Workflow:
1. Connect a client to an ImandraX server with an API key. This opens a session.
2. Admit IML definitions into the session with `eval_src`.
3. Ask ImandraX about them:
   - `verify_src` proves a goal, or refutes it with a counterexample
   - `instance_src` finds an input satisfying a goal
   - `decompose` runs region decomposition on a function, giving each region's constraints, invariant and a sample input / output
   - Alternatively, put `theorem` / `verify` / `instance` / `[@@decomp ...]` in the source given to `eval_src` and read their results from its response
4. Results come back as typed objects: the outcome (proved / refuted, counterexample, instance, regions) is on the response itself. Each result also carries the server task that produced it. Further data (task result, proof, the MIR db of declarations the task ran with) is fetched from the server from the task (e.g. `res.task.db`), or by artifact kind with `list_artifacts` / `get_artifact`.

The snippets below were checked on 2026-11-27 against `imandrax-api` 0.21.0.1 and `@imandra/imandrax-api` 0.21.0. The API may have changed since; check the latest docs and source.

```python
import os
from imandrax_api.v2 import Client, url_prod

api_key = os.environ.get("IMANDRAX_API_KEY") or os.environ.get("IMANDRA_UNI_KEY")

with Client(url=url_prod, api_key=api_key) as client:
    # Admit IML into the session
    client.eval_src("let f x = if x > 0 then x + 1 else 0")

    # Prove / refute
    res = client.verify_src("fun x -> f x > x")
    res.proved, res.refuted, res.counter_example  # False, True, module M = struct let x = 0 end

    # Find an instance
    res = client.instance_src("fun x -> f x = 43")
    res.instance  # module M = struct let x = 42 end

    # Statements in the source: `theorem` / `verify` / `instance` results in `po_results`,
    # `[@@decomp ...]` results in `decomp_results`
    res = client.eval_src("theorem f_nonneg x = f x >= 0")
    for po in res.po_results:
        po.origin.from_sym, po.outcome  # "f_nonneg", proof

    # Region decomposition
    res = client.decompose("f", prune=True, string_results=True)
    for region in res.decomposition.regions:
        region.constraints_str  # ("x <= 0",)
        region.invariant_str    # "0"
        region.sample()         # {Uid(x/...): 0}; key it by name with `uid.short_name`
        region.sample_output()  # 0

    # MIR db: the declarations the task ran with. Uids are `<name>/<hash>`
    db = res.task.db
    db.find_function_re("^f/")  # {Uid(f/...): FunctionDefinition(let f x = ...)}
```

```ts
import { Client } from "@imandra/imandrax-api";

const apiKey = process.env.IMANDRAX_API_KEY || process.env.IMANDRA_UNI_KEY;
await using client = await Client.connect({ apiKey });

await client.evalSrc("let f x = if x > 0 then x + 1 else 0");
const v = await client.verifySrc("fun x -> f x > x");
v.refuted; v.counterExample?.src;

const res = await client.decompose("f", { prune: true, stringResults: true });
for (const r of res.decomposition!.regions) {
  r.constraintsStr; r.invariantStr; r.sample(); r.sampleOutput();
}

const db = await res.task!.db(); // task artifacts are fetched on first use, hence async
db.findFunctionRe("^f/");
```

# Other libraries

- `imandrax-api-models` (PyPI)
  - `imandrax_api_models.proto_models`: pydantic model definitions for ImandraX protobuf request and response messages
  - `imandrax_api_models.pp`: pretty-printer for ImandraX binding (decoded from artifacts)
- `iml-query` (PyPI): IML treesitter parser and query utilities. For manipulating IML code.

Source code: GitHub `imandra-ai/imandrax-tools/packages/`
