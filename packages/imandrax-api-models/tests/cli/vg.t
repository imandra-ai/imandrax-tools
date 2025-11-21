setup
  $ cd $TESTDIR

list_vg - list all verification goals
  $ uv run imandrax list-vg data/vg_mixed.iml
  1: verify (6:0-6:40): fun x -> x > 0 ==> double x > x
  2: instance (8:0-8:36): fun x -> x > 0 && x < 100

list_vg - with no VG requests
  $ uv run imandrax list-vg data/type_err.iml

check_vg - check specific verification goal
  $ uv run imandrax check-vg data/verify_refuted.iml --index=1
  Eval success!
  
  =====VG=====
  
  1: verify (fun x -> g x <= 0)
  {
          'model': {
              'm_type': <ModelType.Counter_example: 'Counter_example'>,
              'src': (
                  'module M = struct\n'
                  '\n'
                  '  let x = 0\n'
                  '\n'
                  ' end\n'
              ),
          },
      }




check_vg - with --check-all flag
  $ uv run imandrax check-vg data/vg_mixed.iml --check-all
  Eval success!
  
  =====VG=====
  
  1: verify (fun x -> x > 0 ==> double x > x)
  {
          'proof_pp': (
              '{ id = 1;\n'
              '  concl =\n'
              '  \n'
              '  |----------------------------------------------------------------------\n'
              '   x > 0 ==> double x > x\n'
              '  ;\n'
              '  view =\n'
              '  T_deduction {\n'
              '    premises =\n'
              '    [("p",\n'
              '      [{ id = 0;\n'
              '         concl =\n'
              '         \n'
              '         |----------------------------------------------------------------------\n'
              '          x > 0 ==> double x > x\n'
              '         ; view = T_deduction {premises = []} }\n'
              '        ])\n'
              '      ]}\n'
              '  }'
          ),
      }
  2: instance (fun x -> x > 0 && x < 100)
  {
          'model': {
              'm_type': <ModelType.Instance: 'Instance'>,
              'src': (
                  'module M = struct\n'
                  '\n'
                  '  let x = 1\n'
                  '\n'
                  ' end\n'
              ),
          },
      }


