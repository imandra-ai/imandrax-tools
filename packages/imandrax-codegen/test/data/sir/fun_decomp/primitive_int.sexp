(((name test_1) (f_name classify_number)
  (f_args ((x (TBase int) (VConst (CInt 1)))))
  (f_output ((TBase int) (VConst (CInt 1))))
  (docstr
    "test_1\
   \n\
   \n- invariant: 1\
   \n- constraints:\
   \n    - not (x = 0)\
   \n    - x >= 1\
   \n"))
 ((name test_2) (f_name classify_number)
  (f_args ((x (TBase int) (VConst (CInt 0)))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr  "test_2\
          \n\
          \n- invariant: 0\
          \n- constraints:\
          \n    - x = 0\
          \n"))
 ((name test_3) (f_name classify_number)
  (f_args ((x (TBase int) (VConst (CInt -1)))))
  (f_output ((TBase int) (VConst (CInt -1))))
  (docstr
    "test_3\
   \n\
   \n- invariant: (-1)\
   \n- constraints:\
   \n    - x <= (-1)\
   \n")))