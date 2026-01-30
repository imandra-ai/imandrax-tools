(((name test_1) (f_name bool_logic)
  (f_args
   ((a (TBase bool) (VConst (CBool false)))
    (b (TBase bool) (VConst (CBool false)))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_1\
   \n\
   \n- invariant: 0\
   \n- constraints:\
   \n    - not a\
   \n    - not b\
   \n"))
 ((name test_2) (f_name bool_logic)
  (f_args
   ((a (TBase bool) (VConst (CBool false)))
    (b (TBase bool) (VConst (CBool true)))))
  (f_output ((TBase int) (VConst (CInt 2))))
  (docstr
    "test_2\
   \n\
   \n- invariant: 2\
   \n- constraints:\
   \n    - b\
   \n    - not a\
   \n"))
 ((name test_3) (f_name bool_logic)
  (f_args
   ((a (TBase bool) (VConst (CBool true)))
    (b (TBase bool) (VConst (CBool false)))))
  (f_output ((TBase int) (VConst (CInt 2))))
  (docstr
    "test_3\
   \n\
   \n- invariant: 2\
   \n- constraints:\
   \n    - a\
   \n    - not b\
   \n"))
 ((name test_4) (f_name bool_logic)
  (f_args
   ((a (TBase bool) (VConst (CBool true)))
    (b (TBase bool) (VConst (CBool true)))))
  (f_output ((TBase int) (VConst (CInt 1))))
  (docstr
    "test_4\
   \n\
   \n- invariant: 1\
   \n- constraints:\
   \n    - a\
   \n    - b\
   \n")))