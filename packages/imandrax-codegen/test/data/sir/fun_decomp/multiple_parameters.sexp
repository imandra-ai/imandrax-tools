(((name test_1) (f_name calculate)
  (f_args
   ((b (TBase int) (VConst (CInt 1))) (c (TBase int) (VConst (CInt 2)))
    (a (TBase int) (VConst (CInt 0)))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_1\
   \n\
   \n- invariant: 0\
   \n- constraints:\
   \n    - a <= b\
   \n    - not (a = b)\
   \n    - not (b = c)\
   \n"))
 ((name test_2) (f_name calculate)
  (f_args
   ((a (TBase int) (VConst (CInt 0))) (c (TBase int) (VConst (CInt 1)))
    (b (TBase int) (VConst (CInt 1)))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_2\
   \n\
   \n- invariant: b * a\
   \n- constraints:\
   \n    - b = c\
   \n    - a <= b\
   \n    - not (a = b)\
   \n"))
 ((name test_3) (f_name calculate)
  (f_args
   ((b (TBase int) (VConst (CInt 0))) (a (TBase int) (VConst (CInt 0)))
    (c (TBase int) (VConst (CInt 0)))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_3\
   \n\
   \n- invariant: a * c\
   \n- constraints:\
   \n    - a = b\
   \n    - a <= b\
   \n"))
 ((name test_4) (f_name calculate)
  (f_args
   ((b (TBase int) (VConst (CInt 0))) (a (TBase int) (VConst (CInt 1)))
    (c (TBase int) (VConst (CInt 1)))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_4\
   \n\
   \n- invariant: 0\
   \n- constraints:\
   \n    - not (a <= b)\
   \n    - b <= c\
   \n    - not (a = b)\
   \n    - not (b = c)\
   \n"))
 ((name test_5) (f_name calculate)
  (f_args
   ((a (TBase int) (VConst (CInt 0))) (c (TBase int) (VConst (CInt -1)))
    (b (TBase int) (VConst (CInt -1)))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_5\
   \n\
   \n- invariant: b * a\
   \n- constraints:\
   \n    - not (a <= b)\
   \n    - b = c\
   \n    - b <= c\
   \n    - not (a = b)\
   \n"))
 ((name test_6) (f_name calculate)
  (f_args
   ((a (TBase int) (VConst (CInt 1))) (c (TBase int) (VConst (CInt -1)))
    (b (TBase int) (VConst (CInt 0)))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_6\
   \n\
   \n- invariant: a + b + c\
   \n- constraints:\
   \n    - not (a <= b)\
   \n    - not (b <= c)\
   \n")))