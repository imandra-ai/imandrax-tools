from imandrax_api_models import EvalRes
from imandrax_tools.iml_eval_corpus.common import BaseDiag

from .decomp_asm_sig_mismatch.query import check as check_decomp_asm_sig_mismatch
from .function_in_composite_type.query import check as check_function_in_composite_type
from .general_termination_proof_error.query import (
    check as check_general_termination_proof_error,
)
from .infix_op_missing_paren.query import check as check_infix_op_missing_paren
from .measure_type_err.query import check as check_measure_type_err
from .nested_measure.query import check as check_nested_measure
from .nested_rec.query import check as check_nested_rec
from .unknown_id_ocaml_stdlib_array.query import (
    check as check_unknown_id_ocaml_stdlib_array,
)
from .unknown_id_ocaml_stdlib_failwith.query import (
    check as check_unknown_id_ocaml_stdlib_failwith,
)
from .unknown_id_ocaml_stdlib_list_fold_left2.query import (
    check as check_unknown_id_ocaml_stdlib_list_fold_left2,
)
from .unknown_id_ocaml_stdlib_option_bind.query import (
    check as check_unknown_id_ocaml_stdlib_option_bind,
)
from .unknown_id_ocaml_stdlib_option_map2.query import (
    check as check_unknown_id_ocaml_stdlib_option_map2,
)
from .unknown_id_ocaml_stdlib_ref.query import (
    check as check_unknown_id_ocaml_stdlib_ref,
)
from .unknown_id_ocaml_stdlib_sys_int_size.query import (
    check as check_unknown_id_ocaml_stdlib_sys_int_size,
)

check_fns = (
    check_decomp_asm_sig_mismatch,
    check_function_in_composite_type,
    check_general_termination_proof_error,
    check_infix_op_missing_paren,
    check_measure_type_err,
    check_nested_measure,
    check_nested_rec,
    check_unknown_id_ocaml_stdlib_array,
    check_unknown_id_ocaml_stdlib_failwith,
    check_unknown_id_ocaml_stdlib_list_fold_left2,
    check_unknown_id_ocaml_stdlib_option_bind,
    check_unknown_id_ocaml_stdlib_option_map2,
    check_unknown_id_ocaml_stdlib_ref,
    check_unknown_id_ocaml_stdlib_sys_int_size,
)

"""
imports=""
checks=""

while IFS= read -r dir; do
    imports+="from .${dir}.query import check as check_${dir}"$'\n'
    checks+="    check_${dir},"$'\n'
done < <(fd --max-depth=1 -t d --exclude "_*" --format '{/}')

echo "$imports"
echo "check_fns = ("
echo "$checks"
echo ")"
"""


def check_all(iml: str, eval_res: EvalRes) -> list[BaseDiag]:
    diags: list[BaseDiag] = []
    for check_fn in check_fns:
        diag_opt = check_fn(iml, eval_res)
        if (diag := diag_opt) is not None:
            diags.append(diag)
    return diags
