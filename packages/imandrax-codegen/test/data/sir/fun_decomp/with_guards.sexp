(((name test_1) (f_name classify)
  (f_args
   ((x (TBase int) (VConst (CInt 0))) (y (TBase int) (VConst (CInt 0)))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr  "test_1\
          \n\
          \n- invariant: 0\
          \n- constraints:\
          \n    - x <= 0\
          \n"))
 ((name test_2) (f_name classify)
  (f_args
   ((x (TBase int) (VConst (CInt 1))) (y (TBase int) (VConst (CInt 0)))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_2\
   \n\
   \n- invariant: 0\
   \n- constraints:\
   \n    - x >= 1\
   \n    - y <= 0\
   \n"))
 ((name test_3) (f_name classify)
  (f_args
   ((y (TBase int) (VConst (CInt 1))) (x (TBase int) (VConst (CInt 1)))))
  (f_output ((TBase int) (VConst (CInt 3))))
  (docstr
    "test_3\
   \n\
   \n- invariant: 3\
   \n- constraints:\
   \n    - x <= y\
   \n    - y <= x\
   \n    - x >= 1\
   \n    - y >= 1\
   \n"))
 ((name test_4) (f_name classify)
  (f_args
   ((y (TBase int) (VConst (CInt 2))) (x (TBase int) (VConst (CInt 1)))))
  (f_output ((TBase int) (VConst (CInt 2))))
  (docstr
    "test_4\
   \n\
   \n- invariant: 2\
   \n- constraints:\
   \n    - not (y <= x)\
   \n    - x <= y\
   \n    - x >= 1\
   \n    - y >= 1\
   \n"))
 ((name test_5) (f_name classify)
  (f_args
   ((y (TBase int) (VConst (CInt 1))) (x (TBase int) (VConst (CInt 2)))))
  (f_output ((TBase int) (VConst (CInt 1))))
  (docstr
    "test_5\
   \n\
   \n- invariant: 1\
   \n- constraints:\
   \n    - not (x <= y)\
   \n    - x >= 1\
   \n    - y >= 1\
   \n")))