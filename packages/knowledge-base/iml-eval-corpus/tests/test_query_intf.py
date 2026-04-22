"""Static-only Protocol conformance check for every corpus category.

Not a pytest module (leading underscore) — pyright type-checks it at CI
time and flags any category whose `query` package drifts from
`QueryModule`. Adding a new category? Add a line below.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from imandrax_tools.iml_eval_corpus.query_protocol import QueryModule

    from corpus.decomp_asm_sig_mismatch import query as _decomp_asm_sig_mismatch
    from corpus.function_in_composite_type import query as _function_in_composite_type
    from corpus.general_termination_proof_error import (
        query as _general_termination_proof_error,
    )
    from corpus.infix_op_missing_paren import query as _infix_op_missing_paren
    from corpus.measure_type_err import query as _measure_type_err
    from corpus.nested_measure import query as _nested_measure
    from corpus.nested_rec import query as _nested_rec
    from corpus.unknown_id_ocaml_stdlib_array import (
        query as _unknown_id_ocaml_stdlib_array,
    )
    from corpus.unknown_id_ocaml_stdlib_failwith import (
        query as _unknown_id_ocaml_stdlib_failwith,
    )
    from corpus.unknown_id_ocaml_stdlib_list_fold_left2 import (
        query as _unknown_id_ocaml_stdlib_list_fold_left2,
    )
    from corpus.unknown_id_ocaml_stdlib_option_bind import (
        query as _unknown_id_ocaml_stdlib_option_bind,
    )
    from corpus.unknown_id_ocaml_stdlib_option_map2 import (
        query as _unknown_id_ocaml_stdlib_option_map2,
    )
    from corpus.unknown_id_ocaml_stdlib_ref import (
        query as _unknown_id_ocaml_stdlib_ref,
    )
    from corpus.unknown_id_ocaml_stdlib_sys_int_size import (
        query as _unknown_id_ocaml_stdlib_sys_int_size,
    )

    _CHECKS: list[QueryModule] = [
        _decomp_asm_sig_mismatch,
        _function_in_composite_type,
        _general_termination_proof_error,
        _infix_op_missing_paren,
        _measure_type_err,
        _nested_measure,
        _nested_rec,
        _unknown_id_ocaml_stdlib_array,
        _unknown_id_ocaml_stdlib_failwith,
        _unknown_id_ocaml_stdlib_list_fold_left2,
        _unknown_id_ocaml_stdlib_option_bind,
        _unknown_id_ocaml_stdlib_option_map2,
        _unknown_id_ocaml_stdlib_ref,
        _unknown_id_ocaml_stdlib_sys_int_size,
    ]

__test__: bool = False
