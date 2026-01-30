(((name test_1) (f_name nested_check)
  (f_args
   ((x (TBase int) (VConst (CInt 0))) (y (TBase int) (VConst (CInt 0)))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_1\
   \n\
   \n- invariant: ~- x + y\
   \n- constraints:\
   \n    - x <= 0\
   \n    - y <= 0\
   \n"))
 ((name test_2) (f_name nested_check)
  (f_args
   ((y (TBase int) (VConst (CInt 1))) (x (TBase int) (VConst (CInt 0)))))
  (f_output ((TBase int) (VConst (CInt 1))))
  (docstr
    "test_2\
   \n\
   \n- invariant: y - x\
   \n- constraints:\
   \n    - y >= 1\
   \n    - x <= 0\
   \n"))
 ((name test_3) (f_name nested_check)
  (f_args
   ((x (TBase int) (VConst (CInt 1))) (y (TBase int) (VConst (CInt 0)))))
  (f_output ((TBase int) (VConst (CInt 1))))
  (docstr
    "test_3\
   \n\
   \n- invariant: x - y\
   \n- constraints:\
   \n    - x >= 1\
   \n    - y <= 0\
   \n"))
 ((name test_4) (f_name nested_check)
  (f_args
   ((x (TBase int) (VConst (CInt 1))) (y (TBase int) (VConst (CInt 1)))))
  (f_output ((TBase int) (VConst (CInt 2))))
  (docstr
    "test_4\
   \n\
   \n- invariant: x + y\
   \n- constraints:\
   \n    - x >= 1\
   \n    - y >= 1\
   \n")))