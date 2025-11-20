setup
  $ cd $TESTDIR

format_error
  $ uv run imandrax data/type_err.iml
  Evaluation errors:
  
  <error_1>
  Lines: 3:17-3:20
  Error: Expression is expected to have type `int`,
  but its inferred type is `bool`.
  The subtypes `int` and `bool` do not match.
  
    1 | let good = fun x -> x + 1
    2 | 
  * 3 | let bad : int = true
      |                  ^^^
    4 | 
    5 | let good_again = fun x -> x
  <kind>{ Kind.name = "TypeErr" }</kind>
  </error_1>
  



format_po_error
  $ uv run imandrax data/po_tactic_eval_err.iml
  Evaluation errors:
  
  Proof obligation errors (including termination proving errors):
  <po_error_1>
  Tactic failed: Goal is counter-satisfiable.
  <kind>{ Kind.name = "TacticEvalErr" }</kind>
  </po_error_1>
  <po_error_2>
  Tactic failed: Goal is counter-satisfiable.
  <kind>{ Kind.name = "TacticEvalErr" }</kind>
  </po_error_2>
  <po_error_3>
  Tactic failed: Goal is counter-satisfiable.
  <kind>{ Kind.name = "TacticEvalErr" }</kind>
  </po_error_3>
  
