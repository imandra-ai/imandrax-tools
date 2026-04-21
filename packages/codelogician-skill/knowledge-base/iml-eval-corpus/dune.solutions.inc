; eval solutions and store results
(rule
 (mode (promote (until-clean)))
 (deps (glob_files solution*.iml))
 (target solutions_eval_res.json)
 (action
  (with-stdout-to
   %{target}
   (bash
    "for f in %{deps}; do iml-eval-corpus-imx-eval \"$f\" | jq --arg name \"$f\" '{($name): .}'; done | jq -s 'add // {}'"))))

; test that all solutions has no errors
(rule
 (alias runtest)
 (deps solutions_eval_res.json)
 (action
  (bash "jq -e '[.[] | (.eval_res.errors | length), (.eval_res.po_results[]?.errors | length)] | all(. == 0)' %{deps} > /dev/null")))
