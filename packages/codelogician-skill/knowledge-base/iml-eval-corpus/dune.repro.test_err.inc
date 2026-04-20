(rule
 (alias runtest)
 (deps eval_res.json)
 (action
  (bash "jq -e '(.eval_res.errors | length) > 0' %{deps} > /dev/null")))
