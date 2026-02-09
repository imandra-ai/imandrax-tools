(((name test_1) (f_name area)
  (f_args
   ((s (TBase shape)
     (VConstruct (constructor Rectangle)
      (args ((VConst (CInt 0)) (VConst (CInt 1))))))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_1\
   \n\
   \n- invariant: Destruct(Rectangle, 0, s) * Destruct(Rectangle, 1, s)\
   \n- constraints:\
   \n    - not Is_a(Circle, s)\
   \n"))
 ((name test_2) (f_name area)
  (f_args
   ((s (TBase shape)
     (VConstruct (constructor Circle) (args ((VConst (CInt 0))))))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_2\
   \n\
   \n- invariant: Destruct(Circle, 0, s) * Destruct(Circle, 0, s)\
   \n- constraints:\
   \n    - Is_a(Circle, s)\
   \n")))