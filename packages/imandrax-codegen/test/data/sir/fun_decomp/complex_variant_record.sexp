(((name test_1) (f_name process_user)
  (f_args
   ((u (TBase user)
     (VRecord (type_name user)
      (fields
       ((id (VConst (CInt 0)))
        (active (VConstruct (constructor Inactive) (args ())))))))))
  (f_output ((TBase int) (VConst (CInt -1))))
  (docstr
    "test_1\
   \n\
   \n- invariant: (-1)\
   \n- constraints:\
   \n    - not (u.active = Active)\
   \n"))
 ((name test_2) (f_name process_user)
  (f_args
   ((u (TBase user)
     (VRecord (type_name user)
      (fields
       ((id (VConst (CInt 0)))
        (active (VConstruct (constructor Active) (args ())))))))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_2\
   \n\
   \n- invariant: 0\
   \n- constraints:\
   \n    - u.active = Active\
   \n    - u.id <= 0\
   \n"))
 ((name test_3) (f_name process_user)
  (f_args
   ((u (TBase user)
     (VRecord (type_name user)
      (fields
       ((id (VConst (CInt 1)))
        (active (VConstruct (constructor Active) (args ())))))))))
  (f_output ((TBase int) (VConst (CInt 1))))
  (docstr
    "test_3\
   \n\
   \n- invariant: u.id\
   \n- constraints:\
   \n    - u.active = Active\
   \n    - u.id >= 1\
   \n")))