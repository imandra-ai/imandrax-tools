((var_name w)
 (ty
  (TApp container3
   ((TBase int) (TBase int) (TApp container2 ((TBase int) (TBase int))))))
 (tm
  (VRecord (type_name container3)
   (fields
    ((v
      (VTuple
       ((VConst (CInt 1)) (VConst (CInt 2))
        (VRecord (type_name container2)
         (fields ((v (VTuple ((VConst (CInt 3)) (VConst (CInt 4))))))))))))))))