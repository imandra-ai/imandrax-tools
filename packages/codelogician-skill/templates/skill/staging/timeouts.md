---
name: timeouts
description: HTTP connection timeout and compute budget timeout when communicating with ImandraX (via `codelogician` CLI or Python libs).
tags: [concept, how-to]
---

There exists two types of timeouts:
- an **HTTP timeout** bounds how long the client waits for a reply
- a **compute timeout** bounds how long the server spends on a piece of work

You set them in different places, and they fail in different ways:

|                      | **compute timeout**                                               | **HTTP timeout**                                  |
| -------------------- | ----------------------------------------------------------------- | ------------------------------------------------- |
| you set it with      | `[@@timeout N]` in your IML, or a `compute_timeout` request field | a client option: `--timeout`, `Client(timeout=…)` |
| when it fires        | the server answers, and the PO comes back `Interrupted`           | the client gives up, so you get no answer at all  |
| what the server does | stops that piece of work                                          | keeps working                                     |

NOTE: it's possible that the server will keep worker on a task after you are cut off from the HTTP connection timeout.

## The `[@@timeout]` attribute

Attach `[@@timeout N]` to a top-level definition. `N` counts seconds.

```iml
let rec ackermann m n =
  if m <= 0 then n + 1
  else if n <= 0 then ackermann (m - 1) 1
  else ackermann (m - 1) (ackermann m (n - 1))
[@@measure Ordinal.pair (Ordinal.of_int m) (Ordinal.of_int n)]
[@@timeout 300]
```

The timeout is not a budget for the whole definition, but for each proof-obligation task that is spawned from that definition. Each PO gets its own `N` seconds timeout.


## What a compute timeout looks like

The PO comes back as an error of kind `Interrupted`:

```
`Interrupted`: Computation was interrupted ...
```

## Hard 300-second connection timeout

The gateway caps a connection at roughly 300 seconds, and no client setting raises that. Asking for a 600-second compute timeout on a blocking call therefore doesn't work: the connection dies after about five minutes with a 504 while the server carries on.
