Setup: Define helper function
  $ fence() { printf '```python\n'; cat; printf '```'; }
  $ run_test() { (
  >    cd $DUNE_SOURCEROOT/packages/imandrax-codegen && \
  >    py-gen-parse "tests/data/art/model/$1" - --mode model \
  >    | uv run python/imandrax_codegen/code_of_ast - \
  >    | fence
  > ); }

inline_record
  $ run_test composite/inline_record.yaml
  expected output

map_default_value_only
  $ run_test composite/map_default_value_only.yaml
  expected output

map_int_bool_0
  $ run_test composite/map_int_bool_0.yaml
  expected output

map_int_bool_1
  $ run_test composite/map_int_bool_1.yaml
  expected output

map_int_bool_2
  $ run_test composite/map_int_bool_2.yaml
  expected output

multiset_empty
  $ run_test composite/multiset_empty.yaml
  expected output

multiset_nonempty
  $ run_test composite/multiset_nonempty.yaml
  expected output

set_empty
  $ run_test composite/set_empty.yaml
  expected output

set_nonempty
  $ run_test composite/set_nonempty.yaml
  expected output

variant_and_record
  $ run_test composite/variant_and_record.yaml
  expected output

bool list
  $ run_test primitive/bool_list.yaml
  expected output

empty list
  $ run_test primitive/empty_list.yaml
  expected output

int option none
  $ run_test primitive/int_option_none.yaml
  expected output

int option
  $ run_test primitive/int_option.yaml
  expected output

int
  $ run_test primitive/int.yaml
  expected output

LChar
  $ run_test primitive/LChar.yaml
  expected output

LString
  $ run_test primitive/LString.yaml
  expected output

real
  $ run_test primitive/real.yaml
  expected output

record
  $ run_test primitive/record.yaml
  expected output

single element int list
  $ run_test primitive/single_element_int_list.yaml
  expected output

tuple of bool and int
  $ run_test primitive/tuple_of_bool_and_int.yaml
  expected output

variant1
  $ run_test primitive/variant1.yaml
  expected output

variant2
  $ run_test primitive/variant2.yaml
  expected output

variant3
  $ run_test primitive/variant3.yaml
  expected output

