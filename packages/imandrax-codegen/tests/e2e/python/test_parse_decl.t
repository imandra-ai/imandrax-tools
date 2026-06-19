Setup: Define helper function
  $ fence() { printf '```python\n'; cat; printf '```'; }
  $ run_test() { (
  >    cd $DUNE_SOURCEROOT/packages/imandrax-codegen && \
  >    yq ".get_decls_res.decls[0].artifact" "tests/data/art/decl/$1" -o json \
  >    | py-gen-parse - - --mode decl \
  >    | uv run python/imandrax_codegen/code_of_ast - \
  >    | fence
  > ); }

function1
  $ run_test function1.yaml
  expected output

function2
  $ run_test function2.yaml
  expected output

function3
  $ run_test function3.yaml
  expected output

function4
  $ run_test function4.yaml
  expected output

function5
  $ run_test function5.yaml
  expected output

function6
  $ run_test function6.yaml
  expected output

function7
  $ run_test function7.yaml
  expected output

GADT_monomorphic
  $ run_test GADT_monomorphic.yaml
  expected output

nested_generics
  $ run_test nested_generics.yaml
  expected output

real_and_option
  $ run_test real_and_option.yaml
  expected output

record_with_composite_type
  $ run_test record_with_composite_type.yaml
  expected output

record
  $ run_test record.yaml
  expected output

tuple_two_int
  $ run_test tuple_two_int.yaml
  expected output

variant_poly_two_var
  $ run_test variant_poly_two_var.yaml
  expected output

variant_poly
  $ run_test variant_poly.yaml
  expected output

variant_recursive
  $ run_test variant_recursive.yaml
  expected output

variant_simple
  $ run_test variant_simple.yaml
  expected output

variant_two
  $ run_test variant_two.yaml
  expected output

variant_with_composite_payload
  $ run_test variant_with_composite_payload.yaml
  expected output

variant_with_payload
  $ run_test variant_with_payload.yaml
  expected output

