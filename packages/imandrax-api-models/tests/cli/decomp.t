setup
  $ cd $TESTDIR

list_decomp - list all decompose requests
  $ uv run imandrax list-decomp data/decompose_simple.iml
  1: simple_branch
  2: conditional_fn

list_decomp - with no decompose requests
  $ uv run imandrax list-decomp data/type_err.iml

check_decomp - check specific decompose request
  $ uv run imandrax check-decomp data/decompose_simple.iml --index=1
  Eval success!
  
  =====Decomp=====
  
  1: decompose simple_branch
  {
          'err': None,
          'errors': [],
          'task': {
              'id': {
                  'id': 'task:decomp:Fep5rN9wmfKzji3P_Oxg3m2QCblNPo7074PhiwEpS1Q=',
              },
              'kind': <TaskKind.TASK_DECOMP: 'TASK_DECOMP'>,
          },
          'regions_str': [
              {
                  'constraints_str': [
                      'not (x = 2)',
                      'not (x = 1)',
                  ],
                  'invariant_str': 'x - 1',
                  'model_str': {'x': '0'},
                  'model_eval_str': '(-1)',
              },
              {
                  'constraints_str': [
                      'not (x = 1)',
                      'x = 2',
                  ],
                  'invariant_str': 'x + 1',
                  'model_str': {'x': '2'},
                  'model_eval_str': '3',
              },
              {
                  'constraints_str': ['x = 1'],
                  'invariant_str': 'x + 1',
                  'model_str': {'x': '1'},
                  'model_eval_str': '2',
              },
          ],
      }



check_decomp - with --check-all flag
  $ uv run imandrax check-decomp data/decompose_simple.iml --check-all
  Eval success!
  
  =====Decomp=====
  
  1: decompose simple_branch
  2: decompose conditional_fn
  {
          'err': None,
          'errors': [],
          'task': {
              'id': {
                  'id': 'task:decomp:Fep5rN9wmfKzji3P_Oxg3m2QCblNPo7074PhiwEpS1Q=',
              },
              'kind': <TaskKind.TASK_DECOMP: 'TASK_DECOMP'>,
          },
          'regions_str': [
              {
                  'constraints_str': [
                      'not (x = 2)',
                      'not (x = 1)',
                  ],
                  'invariant_str': 'x - 1',
                  'model_str': {'x': '0'},
                  'model_eval_str': '(-1)',
              },
              {
                  'constraints_str': [
                      'not (x = 1)',
                      'x = 2',
                  ],
                  'invariant_str': 'x + 1',
                  'model_str': {'x': '2'},
                  'model_eval_str': '3',
              },
              {
                  'constraints_str': ['x = 1'],
                  'invariant_str': 'x + 1',
                  'model_str': {'x': '1'},
                  'model_eval_str': '2',
              },
          ],
      }
  {
          'err': None,
          'errors': [],
          'task': {
              'id': {
                  'id': 'task:decomp:-90X9q5FgXFwVbCoSV6FMKv2-W56nmgdrzz_IZdvCNw=',
              },
              'kind': <TaskKind.TASK_DECOMP: 'TASK_DECOMP'>,
          },
          'regions_str': [
              {
                  'constraints_str': [
                      'y <= 10',
                  ],
                  'invariant_str': '0',
                  'model_str': {
                      'y': '10',
                  },
                  'model_eval_str': '0',
              },
              {
                  'constraints_str': [
                      'y >= 11',
                  ],
                  'invariant_str': 'y',
                  'model_str': {
                      'y': '11',
                  },
                  'model_eval_str': '11',
              },
          ],
      }
