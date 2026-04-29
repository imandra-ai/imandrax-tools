# iml-eval-corpus

Each directory in `corpus/` except for the ones that has leading `_` in names represents one category of errors we'd like to catch and provide explanation or solution that will be helpful for fixing the error.

Directory structure validity for subdirectories in `corpus/`:
  - error type tags for `repro.iml` and its `eval_res.json`:
  - `.is_po_error`: error is expected in `po_results[].errors` only
  - `.error_in_msg`: error is expected in `messages` only
  - no tag: error is expected in `eval_res.errors` only
  - `solution*.iml`: solution to the error, if present. It is not expected to have errors.
  - `query/__init__.py`: a Python module that should satisfies sigature of `QueryModule` (see `tests/test_query_intf.py`).
    - it should provides data for diagnostic rule and check function (which helps to catch the error)

See corresponding dune files for how to generate `eval_res.json` and `solutions_eval_res.json` from corresponding `repro.iml` and `solution*.iml`.
Also see dune rules which attaches to `runtest` alias for validating the directory structure.

## Processs of adding a new error category

1. Add a new directory in `corpus/`
2. Add a new `repro.iml` file
3. Use dune to generate `eval_res.json`, which should contains the expected (po-)error(s)
4. Add a new `query/__init__.py` file, which should satisfies sigature of `QueryModule`
5. Add `solution*.iml` files, `explanation.md`, and other supporting files