; Shared rules for a corpus entry's repro.iml.
; Generates eval_res.json and asserts the eval reported an error.

(rule
 (mode (promote (until-clean)))
 (deps (:in repro.iml))
 (target eval_res.json)
 (action
  (with-stdout-to
   %{target}
   (run iml-eval-corpus-imx-eval %{in}))))

(rule
 (alias runtest)
 (deps eval_res.json)
 (action
  (bash "jq -e '(.eval_res.errors | length) > 0' %{deps} > /dev/null")))
