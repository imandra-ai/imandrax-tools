((Infeasible (name test_1) (f_name check)
  (docstr
    "test_1\
   \n\
   \n- invariant: None\
   \n- constraints:\
   \n    - not (List.for_all always_true xs)\
   \n")
  (reason "{ reason = \"max number of steps (100) reached\" }"))
 (Feasible (name test_2) (f_name check)
  (f_args ((xs (TApp list ((TBase int))) (VList ()))))
  (f_output
   ((TApp option ((TApp list ((TBase int)))))
    (VConstruct (constructor Some) (args ((VList ()))))))
  (docstr
    "test_2\
   \n\
   \n- invariant: Some xs\
   \n- constraints:\
   \n    - List.for_all always_true xs\
   \n")))