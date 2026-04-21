(rule
 (alias runtest)
 (deps eval_res.json .is_po_error)
 (action
  (bash "jq -e '.eval_res.po_results | any(.[]; .errors | length > 0)' %{deps} > /dev/null")))
