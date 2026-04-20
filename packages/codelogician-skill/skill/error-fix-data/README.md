---
name: Error-Fix Database
description: IML error and fixes database. Provides `error_corpus.json`, a collection of common IML errors and their fixes. Search it using jq or grep to find relevant errors and their fixes.
---

This directory contains `iml_eval_corpus.json`, a collection of common IML errors and their fixes. You can search it using `jq`, `grep`, etc. to find relevant errors and their fixes.

Each entry in `items` has:

- `name` — entry identifier.
- `repro` — the `repro.iml` source that triggers the error.
- `.eval_res | (.errors[0] // .po_results[0].errors[0]) | .msg` — the error message object (`msg`, `locs`, `backtrace`) extracted from `eval_res.json` via that jq query. To access this field in jq, quote it, e.g. `jq '.items[0]["'"'"'.eval_res | (.errors[0] // .po_results[0].errors[0]) | .msg"'"'"']'`.
- `is_po_err` — `true` if the error is a proof-obligation error (marked via a `.is_po_error` file in the source entry).
- `solution` — the fixed IML source.
- `explanation` — prose explanation of the error and the fix.
