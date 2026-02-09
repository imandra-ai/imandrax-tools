(((name test_1) (f_name distance_category)
  (f_args
   ((p (TBase point)
     (VRecord (type_name point)
      (fields ((x (VConst (CInt 0))) (y (VConst (CInt 1)))))))))
  (f_output ((TBase str) (VConst (CString positive))))
  (docstr
    "test_1\
   \n\
   \n- invariant: \"positive\"\
   \n- constraints:\
   \n    - not (p.x + p.y = 0)\
   \n    - p.x + p.y >= 1\
   \n"))
 ((name test_2) (f_name distance_category)
  (f_args
   ((p (TBase point)
     (VRecord (type_name point)
      (fields ((x (VConst (CInt -38))) (y (VConst (CInt 38)))))))))
  (f_output ((TBase str) (VConst (CString origin))))
  (docstr
    "test_2\
   \n\
   \n- invariant: \"origin\"\
   \n- constraints:\
   \n    - p.x + p.y = 0\
   \n"))
 ((name test_3) (f_name distance_category)
  (f_args
   ((p (TBase point)
     (VRecord (type_name point)
      (fields ((x (VConst (CInt 0))) (y (VConst (CInt -1)))))))))
  (f_output ((TBase str) (VConst (CString negative))))
  (docstr
    "test_3\
   \n\
   \n- invariant: \"negative\"\
   \n- constraints:\
   \n    - p.x + p.y <= (-1)\
   \n")))