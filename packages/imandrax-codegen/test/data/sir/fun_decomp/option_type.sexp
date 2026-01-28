(((name test_1) (f_name option_value)
  (f_args
   ((opt (TApp option ((TBase int)))
     (VConstruct (constructor Some) (args ((VConst (CInt 0))))))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_1\
   \n\
   \n- invariant: ~- Option.get opt\
   \n- constraints:\
   \n    - not Is_a(None, opt)\
   \n    - Option.get opt <= 0\
   \n"))
 ((name test_2) (f_name option_value)
  (f_args
   ((opt (TApp option ((TBase int)))
     (VConstruct (constructor Some) (args ((VConst (CInt 1))))))))
  (f_output ((TBase int) (VConst (CInt 1))))
  (docstr
    "test_2\
   \n\
   \n- invariant: Option.get opt\
   \n- constraints:\
   \n    - not Is_a(None, opt)\
   \n    - Option.get opt >= 1\
   \n"))
 ((name test_3) (f_name option_value)
  (f_args ((opt (TApp option ((TBase int))) (VConst CUnit))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_3\
   \n\
   \n- invariant: 0\
   \n- constraints:\
   \n    - Is_a(None, opt)\
   \n")))