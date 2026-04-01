((var_name w) (ty (TApp Map.t ((TBase int) (TBase bool))))
 (tm
  (VMap (default (VConst (CBool false)))
   (entries
    (((VConst (CInt 1)) (VConst (CBool true)))
     ((VConst (CInt 3)) (VConst (CBool true)))
     ((VConst (CInt 2)) (VConst (CBool true))))))))