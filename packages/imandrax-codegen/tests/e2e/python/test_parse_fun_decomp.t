Setup: Define helper function
  $ fence() { printf '```python\n'; cat; printf '```'; }
  $ run_test() { (
  >    cd $DUNE_SOURCEROOT/packages/imandrax-codegen && \
  >    yq ".decomp_res.artifact" "tests/data/art/fun_decomp/$1" -o json \
  >    | py-gen-parse - - --mode fun-decomp \
  >    | uv run python/imandrax_codegen/code_of_ast - \
  >    | fence
  > ); }

basic
  $ run_test basic.yaml
  expected output

complex_variant_record
  $ run_test complex_variant_record.yaml
  expected output

composite_record
  $ run_test composite_record.yaml
  expected output

composite_tuple
  $ run_test composite_tuple.yaml
  expected output

infeasible_region_in_trivial_forall
  $ run_test infeasible_region_in_trivial_forall.yaml
  expected output

multiple_parameters
  $ run_test multiple_parameters.yaml
  expected output

nested_conditions
  $ run_test nested_conditions.yaml
  expected output

option_type
  $ run_test option_type.yaml
  expected output

primitive_bool
  $ run_test primitive_bool.yaml
  expected output

primitive_int
  $ run_test primitive_int.yaml
  expected output

primitive_real
  $ run_test primitive_real.yaml
  expected output

variant_poly
  $ run_test variant_poly.yaml
  expected output

variant_simple
  $ run_test variant_simple.yaml
  expected output

variant_with_data
  $ run_test variant_with_data.yaml
  expected output

with_basis
  $ run_test with_basis.yaml
  expected output

with_guards
  $ run_test with_guards.yaml
  expected output

