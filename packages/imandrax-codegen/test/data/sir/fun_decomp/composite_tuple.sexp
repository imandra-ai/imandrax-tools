(((name test_1) (f_name tuple_compare)
  (f_args
   ((_x_1_25 (TTuple ((TBase int) (TBase int)))
     (VTuple ((VConst (CInt 0)) (VConst (CInt 1)))))))
  (f_output ((TBase int) (VConst (CInt 1))))
  (docstr
    "test_1\
   \n\
   \n- invariant: _x_1_25.1 - _x_1_25.0\
   \n- constraints:\
   \n    - _x_1_25.0 <= _x_1_25.1\
   \n    - not (_x_1_25.0 = _x_1_25.1)\
   \n"))
 ((name test_2) (f_name tuple_compare)
  (f_args
   ((_x_1_25 (TTuple ((TBase int) (TBase int)))
     (VTuple ((VConst (CInt 0)) (VConst (CInt 0)))))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_2\
   \n\
   \n- invariant: 0\
   \n- constraints:\
   \n    - _x_1_25.0 = _x_1_25.1\
   \n    - _x_1_25.0 <= _x_1_25.1\
   \n"))
 ((name test_3) (f_name tuple_compare)
  (f_args
   ((_x_1_25 (TTuple ((TBase int) (TBase int)))
     (VTuple ((VConst (CInt 0)) (VConst (CInt -1)))))))
  (f_output ((TBase int) (VConst (CInt 1))))
  (docstr
    "test_3\
   \n\
   \n- invariant: _x_1_25.0 - _x_1_25.1\
   \n- constraints:\
   \n    - not (_x_1_25.0 <= _x_1_25.1)\
   \n")))