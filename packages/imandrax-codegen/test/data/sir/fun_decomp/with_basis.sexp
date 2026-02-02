(((name test_1) (f_name compute) (f_args ((x (TBase int) (VConst (CInt 0)))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_1\
   \n\
   \n- invariant: helper (~- x)\
   \n- constraints:\
   \n    - x <= 0\
   \n"))
 ((name test_2) (f_name compute) (f_args ((x (TBase int) (VConst (CInt 1)))))
  (f_output ((TBase int) (VConst (CInt 1))))
  (docstr
    "test_2\
   \n\
   \n- invariant: helper x\
   \n- constraints:\
   \n    - x >= 1\
   \n")))