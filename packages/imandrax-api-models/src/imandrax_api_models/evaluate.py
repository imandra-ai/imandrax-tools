"""`eval <expr>` helper"""

from .client import ImandraXClient


def evaluate(client: ImandraXClient, iml: str, expr: str, po_skip: bool = True):
    """
    Evaluate `expr` against the model defined by `iml`.

    Raises:
        ValueError: if the model body itself failed to load, in which case
            `expr` would be evaluated against a model that is not there. Only
            non-PO errors count: with `po_skip=False` a failed proof obligation
            is left in the returned `body_eval_res` for the caller to judge.

    """
    if po_skip:
        task_filter = ['anonymous']
    else:
        task_filter = None
    body_eval_res = client.eval_model(iml, task_filter=task_filter)

    # NOTE: `.success` is `True` even when `.errors` is non-empty, so the
    # errors are what we go by.
    if body_eval_res.errors or not body_eval_res.success:
        errors = '\n'.join(
            f'{e.kind}: {e.msg.msg if e.msg else "<no message>"}'
            for e in body_eval_res.errors
        )
        msg = f'model body failed to load, refusing to evaluate {expr!r}'
        raise ValueError(f'{msg}\n{errors}' if errors else msg)

    leval_snippet = f'eval ({expr})'
    leval_res = client.eval_src(leval_snippet)
    return body_eval_res, leval_res
