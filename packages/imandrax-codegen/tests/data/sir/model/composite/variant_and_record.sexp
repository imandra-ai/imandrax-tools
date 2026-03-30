((var_name w) (ty (TBase movement))
 (tm
  (VConstruct (constructor Move)
   (args
    ((VRecord (type_name position)
      (fields
       ((x (VConst (CInt 1))) (y (VConst (CInt 2))) (z (VConst (CFloat 3))))))
     (VConstruct (constructor North) (args ())))))))