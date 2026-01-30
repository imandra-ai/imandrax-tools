(((name test_1) (f_name f)
  (f_args
   ((c (TApp container ((TBase int) (TBase real)))
     (VConstruct (constructor Labeled)
      (args ((VConst (CInt 0)) (VConst (CFloat 0))))))))
  (f_output ((TBase int) (VConst (CInt 3))))
  (docstr
    "test_1\
   \n\
   \n- invariant: 3\
   \n- constraints:\
   \n    - not Is_a(Empty, c)\
   \n    - not Is_a(Pair, c)\
   \n    - not Is_a(Single, c)\
   \n"))
 ((name test_2) (f_name f)
  (f_args
   ((c (TApp container ((TBase int) (TBase real)))
     (VConstruct (constructor Single) (args ((VConst (CInt 0))))))))
  (f_output ((TBase int) (VConst (CInt -1))))
  (docstr
    "test_2\
   \n\
   \n- invariant: (-1)\
   \n- constraints:\
   \n    - Is_a(Single, c)\
   \n    - not Is_a(Empty, c)\
   \n    - not Is_a(Pair, c)\
   \n    - Destruct(Single, 0, c) <= 0\
   \n"))
 ((name test_3) (f_name f)
  (f_args
   ((c (TApp container ((TBase int) (TBase real)))
     (VConstruct (constructor Single) (args ((VConst (CInt 1))))))))
  (f_output ((TBase int) (VConst (CInt 1))))
  (docstr
    "test_3\
   \n\
   \n- invariant: 1\
   \n- constraints:\
   \n    - Is_a(Single, c)\
   \n    - not Is_a(Empty, c)\
   \n    - not Is_a(Pair, c)\
   \n    - Destruct(Single, 0, c) >= 1\
   \n"))
 ((name test_4) (f_name f)
  (f_args
   ((c (TApp container ((TBase int) (TBase real)))
     (VConstruct (constructor Pair)
      (args ((VConst (CInt 0)) (VConst (CFloat 0))))))))
  (f_output ((TBase int) (VConst (CInt -2))))
  (docstr
    "test_4\
   \n\
   \n- invariant: (-2)\
   \n- constraints:\
   \n    - Is_a(Pair, c)\
   \n    - not Is_a(Empty, c)\
   \n    - Real.of_int (Destruct(Pair, 0, c)) <=. Destruct(Pair, 1, c)\
   \n"))
 ((name test_5) (f_name f)
  (f_args
   ((c (TApp container ((TBase int) (TBase real)))
     (VConstruct (constructor Pair)
      (args ((VConst (CInt 0)) (VConst (CFloat -1))))))))
  (f_output ((TBase int) (VConst (CInt 2))))
  (docstr
    "test_5\
   \n\
   \n- invariant: 2\
   \n- constraints:\
   \n    - not (Real.of_int (Destruct(Pair, 0, c)) <=. Destruct(Pair, 1, c))\
   \n    - Is_a(Pair, c)\
   \n    - not Is_a(Empty, c)\
   \n"))
 ((name test_6) (f_name f)
  (f_args
   ((c (TApp container ((TBase int) (TBase real)))
     (VConstruct (constructor Empty) (args ())))))
  (f_output ((TBase int) (VConst (CInt 0))))
  (docstr
    "test_6\
   \n\
   \n- invariant: 0\
   \n- constraints:\
   \n    - Is_a(Empty, c)\
   \n")))