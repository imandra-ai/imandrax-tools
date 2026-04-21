# iml-eval-corpus

Each of directories except for src/ and the ones that has leading `_` in names represents one category of errors we'd like to catch and provide explanation or solution that will be helpful for fixing the error.

Directory structure validity:
- error type tags for `repro.iml` and its `eval_res.json`:
  - `.is_po_error`: error is expected in `po_results[].errors` only
  - `.error_in_msg`: error is expected in `messages` only
  - no tag: error is expected in `eval_res.errors` only
