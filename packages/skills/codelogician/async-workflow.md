---
name: async-workflow
description: Async-only workflow for submitting tasks non-blockingly and polling for results. A common way to tackle timeouts.
tags: [how-to]
---

Most ways to communicating with ImandraX server support async-only workflow, which means submitting tasks non-blockingly and polling for the results (task artifacts)

In codelogician (>=2.19.0), it has `--async-only` flag and `list-artifacts`, `get-artifacts` commands:
- requests submitted with `--async-only` flag will return immediately without waiting for the task to complete. It returns a task ID
- `list-artifacts` and `get-artifacts` commands can be used to poll for the task results using the task ID returned by the `--async-only` request

For Python client, `imandrax-api-models` provides `Client(async_only=True)`.
