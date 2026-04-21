(rule
 (mode (promote (until-clean)))
 (deps (:in repro.iml))
 (target eval_res.json)
 (action
  (with-stdout-to
   %{target}
   (run iml-eval-corpus-imx-eval %{in}))))

; Assert the expected failure in eval_res.json
(rule
(alias runtest)
(deps eval_res.json (glob_files .is_po_error) (glob_files .error_in_msg))
(action
  (bash
  "pwd=$(pwd); \
    if [[ -f .is_po_error ]]; then \
      case=po_error; \
      jq -e '.eval_res.po_results | any(.[]; .errors | length > 0)' eval_res.json > /dev/null \
      || { echo \"FAIL [$case] in $pwd\"; exit 1; } \
    elif [[ -f .error_in_msg ]]; then \
      case=msg_error; \
      jq -e '.eval_res.messages | any(.[]; contains(\"Error\"))' eval_res.json > /dev/null \
      || { echo \"FAIL [$case] in $pwd\"; exit 1; } \
    else \
      case=top_error; \
      jq -e '(.eval_res.errors | length) > 0' eval_res.json > /dev/null \
      || { echo \"FAIL [$case] in $pwd\"; exit 1; } \
    fi")))
