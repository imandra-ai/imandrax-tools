(((name test_1) (f_name color_value)
  (f_args ((c (TBase color) (VConstruct (constructor Red) (args ())))))
  (f_output ((TBase int) (VConst (CInt 1))))
  (docstr
    "test_1\
   \n\
   \n- invariant: 1\
   \n- constraints:\
   \n    - not (c = Blue)\
   \n    - not (c = Green)\
   \n"))
 ((name test_2) (f_name color_value)
  (f_args ((c (TBase color) (VConstruct (constructor Green) (args ())))))
  (f_output ((TBase int) (VConst (CInt 2))))
  (docstr
    "test_2\
   \n\
   \n- invariant: 2\
   \n- constraints:\
   \n    - c = Green\
   \n    - not (c = Blue)\
   \n"))
 ((name test_3) (f_name color_value)
  (f_args ((c (TBase color) (VConstruct (constructor Blue) (args ())))))
  (f_output ((TBase int) (VConst (CInt 3))))
  (docstr  "test_3\
          \n\
          \n- invariant: 3\
          \n- constraints:\
          \n    - c = Blue\
          \n")))