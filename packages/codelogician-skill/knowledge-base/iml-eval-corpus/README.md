# iml-eval-corpus

Directory structure validity
- error type tags for `repro.iml` and its `eval_res.json`:
  - `.is_po_error`: error is expected in `po_results[].errors` only
  - `.error_in_msg`: error is expected in `messages` only
  - no tag: error is expected in `eval_res.errors` only
