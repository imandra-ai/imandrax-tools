(Variant (name shape_poly) (type_params (a))
 (constructors
  (((vc_name Point) (vc_fields ()))
   ((vc_name Circle) (vc_fields ((Positional (TVar a)))))
   ((vc_name Rectangle)
    (vc_fields ((Positional (TVar a)) (Positional (TVar a)))))
   ((vc_name Triangle)
    (vc_fields ((Named a (TVar a)) (Named b (TVar a)) (Named c (TVar a))))))))