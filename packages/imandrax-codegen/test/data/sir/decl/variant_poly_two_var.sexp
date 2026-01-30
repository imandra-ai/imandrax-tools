(Variant (name container) (type_params (a b))
 (constructors
  (((vc_name Empty) (vc_fields ()))
   ((vc_name Single) (vc_fields ((Positional (TVar a)))))
   ((vc_name Pair) (vc_fields ((Positional (TVar a)) (Positional (TVar b)))))
   ((vc_name Labeled)
    (vc_fields ((Named key (TVar a)) (Named value (TVar b)))))
   ((vc_name Multi)
    (vc_fields
     ((Positional (TApp list ((TVar a))))
      (Positional (TApp list ((TVar b))))))))))