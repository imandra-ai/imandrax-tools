; Shared rules for a corpus entry's solution*.iml files.
; Runs eval on each matching file, bundles results into solutions_eval_res.json,
; and asserts every entry reports success.

(rule
 (mode (promote (until-clean)))
 (deps (glob_files solution*.iml))
 (target solutions_eval_res.json)
 (action
  (with-stdout-to
   %{target}
   (bash
    "for f in %{deps}; do iml-eval-corpus-imx-eval \"$f\" | jq --arg name \"$f\" '{($name): .}'; done | jq -s 'add // {}'"))))

(rule
 (alias runtest)
 (deps solutions_eval_res.json)
 (action
  (bash "jq -e '[.[] | .eval_res.errors | length] | all(. == 0)' %{deps} > /dev/null")))
