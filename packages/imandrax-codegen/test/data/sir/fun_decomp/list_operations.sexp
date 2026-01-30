(((name test_1) (f_name list_check)
  (f_args ((xs (TApp list ((TBase int))) (VList ()))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_1\
   \n\
   \n- invariant: 0\
   \n- constraints:\
   \n    - not (xs <> [])\
   \n"))
 ((name test_2) (f_name list_check)
  (f_args ((xs (TApp list ((TBase int))) (VList ((VConst (CInt 0)))))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_2\
   \n\
   \n- invariant: List.hd xs\
   \n- constraints:\
   \n    - xs <> []\
   \n    - not ((List.tl xs) <> [])\
   \n"))
 ((name test_3) (f_name list_check)
  (f_args
   ((xs (TApp list ((TBase int)))
     (VList ((VConst (CInt 1)) (VConst (CInt 0)))))))
  (f_output ((TBase int) (VConst (CInt 1))))
  (docstr
    "test_3\
   \n\
   \n- invariant: List.hd xs + List.hd (List.tl xs)\
   \n- constraints:\
   \n    - (List.tl xs) <> []\
   \n    - xs <> []\
   \n")))