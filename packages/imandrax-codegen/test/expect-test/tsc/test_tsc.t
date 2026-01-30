Setup: Define helper function
  $ run_tsc() { (
  >    cd $DUNE_SOURCEROOT/packages/imandrax-codegen/test/expect-test/tsc && \
  >    pnpm exec tsc "../../data/ts/$1" --noEmit --lib ES2020 --strict 2>&1 || true
  > ); }

decl/nested_generics
  $ run_tsc decl/nested_generics.ts
  ../../data/ts/decl/nested_generics.ts(2,30): error TS2304: Cannot find name 'tagged'.
  ../../data/ts/decl/nested_generics.ts(2,37): error TS2304: Cannot find name 'validated'.
  ../../data/ts/decl/nested_generics.ts(2,47): error TS2304: Cannot find name 'maybe'.
  ../../data/ts/decl/nested_generics.ts(2,53): error TS2304: Cannot find name 'identity'.

decl/real_and_option
  $ run_tsc decl/real_and_option.ts

decl/record_with_composite_type
  $ run_tsc decl/record_with_composite_type.ts

decl/record
  $ run_tsc decl/record.ts

decl/variant_poly_two_var
  $ run_tsc decl/variant_poly_two_var.ts

decl/variant_poly
  $ run_tsc decl/variant_poly.ts

decl/variant_recursive
  $ run_tsc decl/variant_recursive.ts

decl/variant_simple
  $ run_tsc decl/variant_simple.ts

decl/variant_two
  $ run_tsc decl/variant_two.ts
  ../../data/ts/decl/variant_two.ts(3,32): error TS2304: Cannot find name 'rect'.

decl/variant_with_composite_payload
  $ run_tsc decl/variant_with_composite_payload.ts

decl/variant_with_payload
  $ run_tsc decl/variant_with_payload.ts

fun_decomp/basic
  $ run_tsc fun_decomp/basic.ts

fun_decomp/complex_variant_record
  $ run_tsc fun_decomp/complex_variant_record.ts

fun_decomp/composite_record
  $ run_tsc fun_decomp/composite_record.ts

fun_decomp/composite_tuple
  $ run_tsc fun_decomp/composite_tuple.ts

fun_decomp/list_operations
  $ run_tsc fun_decomp/list_operations.ts

fun_decomp/multiple_parameters
  $ run_tsc fun_decomp/multiple_parameters.ts

fun_decomp/nested_conditions
  $ run_tsc fun_decomp/nested_conditions.ts

fun_decomp/option_type
  $ run_tsc fun_decomp/option_type.ts

fun_decomp/primitive_bool
  $ run_tsc fun_decomp/primitive_bool.ts

fun_decomp/primitive_int
  $ run_tsc fun_decomp/primitive_int.ts

fun_decomp/primitive_real
  $ run_tsc fun_decomp/primitive_real.ts

fun_decomp/variant_poly
  $ run_tsc fun_decomp/variant_poly.ts

fun_decomp/variant_simple
  $ run_tsc fun_decomp/variant_simple.ts

fun_decomp/variant_with_data
  $ run_tsc fun_decomp/variant_with_data.ts

fun_decomp/with_basis
  $ run_tsc fun_decomp/with_basis.ts

fun_decomp/with_guards
  $ run_tsc fun_decomp/with_guards.ts

model/composite/inline_record
  $ run_tsc model/composite/inline_record.ts
  ../../data/ts/model/composite/inline_record.ts(1,10): error TS2304: Cannot find name 'event'.

model/composite/map_default_value_only
  $ run_tsc model/composite/map_default_value_only.ts
  ../../data/ts/model/composite/map_default_value_only.ts(75,21): error TS2304: Cannot find name 'a'.

model/composite/map_int_bool_0
  $ run_tsc model/composite/map_int_bool_0.ts

model/composite/map_int_bool_1
  $ run_tsc model/composite/map_int_bool_1.ts
  ../../data/ts/model/composite/map_int_bool_1.ts(75,7): error TS2322: Type 'DefaultMap<number | boolean, number | boolean>' is not assignable to type 'DefaultMap<number, boolean>'.
    Type 'number | boolean' is not assignable to type 'number'.
      Type 'boolean' is not assignable to type 'number'.
  ../../data/ts/model/composite/map_int_bool_1.ts(75,70): error TS2345: Argument of type '(number | boolean)[][]' is not assignable to parameter of type 'Iterable<[number | boolean, number | boolean]>'.
    The types returned by '[Symbol.iterator]().next(...)' are incompatible between these types.
      Type 'IteratorResult<(number | boolean)[], undefined>' is not assignable to type 'IteratorResult<[number | boolean, number | boolean], any>'.
        Type 'IteratorYieldResult<(number | boolean)[]>' is not assignable to type 'IteratorResult<[number | boolean, number | boolean], any>'.
          Type 'IteratorYieldResult<(number | boolean)[]>' is not assignable to type 'IteratorYieldResult<[number | boolean, number | boolean]>'.
            Type '(number | boolean)[]' is not assignable to type '[number | boolean, number | boolean]'.
              Target requires 2 element(s) but source may have fewer.

model/composite/map_int_bool_2
  $ run_tsc model/composite/map_int_bool_2.ts
  ../../data/ts/model/composite/map_int_bool_2.ts(75,7): error TS2322: Type 'DefaultMap<number | boolean, number | boolean>' is not assignable to type 'DefaultMap<number, boolean>'.
    Type 'number | boolean' is not assignable to type 'number'.
      Type 'boolean' is not assignable to type 'number'.
  ../../data/ts/model/composite/map_int_bool_2.ts(75,70): error TS2345: Argument of type '(number | boolean)[][]' is not assignable to parameter of type 'Iterable<[number | boolean, number | boolean]>'.
    The types returned by '[Symbol.iterator]().next(...)' are incompatible between these types.
      Type 'IteratorResult<(number | boolean)[], undefined>' is not assignable to type 'IteratorResult<[number | boolean, number | boolean], any>'.
        Type 'IteratorYieldResult<(number | boolean)[]>' is not assignable to type 'IteratorResult<[number | boolean, number | boolean], any>'.
          Type 'IteratorYieldResult<(number | boolean)[]>' is not assignable to type 'IteratorYieldResult<[number | boolean, number | boolean]>'.
            Type '(number | boolean)[]' is not assignable to type '[number | boolean, number | boolean]'.
              Target requires 2 element(s) but source may have fewer.

model/composite/multiset_empty
  $ run_tsc model/composite/multiset_empty.ts
  ../../data/ts/model/composite/multiset_empty.ts(75,21): error TS2304: Cannot find name 'a'.

model/composite/multiset_nonempty
  $ run_tsc model/composite/multiset_nonempty.ts
  ../../data/ts/model/composite/multiset_nonempty.ts(75,65): error TS2345: Argument of type 'number[][]' is not assignable to parameter of type 'Iterable<[number, number]>'.
    The types returned by '[Symbol.iterator]().next(...)' are incompatible between these types.
      Type 'IteratorResult<number[], undefined>' is not assignable to type 'IteratorResult<[number, number], any>'.
        Type 'IteratorYieldResult<number[]>' is not assignable to type 'IteratorResult<[number, number], any>'.
          Type 'IteratorYieldResult<number[]>' is not assignable to type 'IteratorYieldResult<[number, number]>'.
            Type 'number[]' is not assignable to type '[number, number]'.
              Target requires 2 element(s) but source may have fewer.

model/composite/set_empty
  $ run_tsc model/composite/set_empty.ts
  ../../data/ts/model/composite/set_empty.ts(75,21): error TS2304: Cannot find name 'a'.

model/composite/set_nonempty
  $ run_tsc model/composite/set_nonempty.ts
  ../../data/ts/model/composite/set_nonempty.ts(75,7): error TS2322: Type 'DefaultMap<number | boolean, number | boolean>' is not assignable to type 'DefaultMap<number, boolean>'.
    Type 'number | boolean' is not assignable to type 'number'.
      Type 'boolean' is not assignable to type 'number'.
  ../../data/ts/model/composite/set_nonempty.ts(75,70): error TS2345: Argument of type '(number | boolean)[][]' is not assignable to parameter of type 'Iterable<[number | boolean, number | boolean]>'.
    The types returned by '[Symbol.iterator]().next(...)' are incompatible between these types.
      Type 'IteratorResult<(number | boolean)[], undefined>' is not assignable to type 'IteratorResult<[number | boolean, number | boolean], any>'.
        Type 'IteratorYieldResult<(number | boolean)[]>' is not assignable to type 'IteratorResult<[number | boolean, number | boolean], any>'.
          Type 'IteratorYieldResult<(number | boolean)[]>' is not assignable to type 'IteratorYieldResult<[number | boolean, number | boolean]>'.
            Type '(number | boolean)[]' is not assignable to type '[number | boolean, number | boolean]'.
              Target requires 2 element(s) but source may have fewer.

model/composite/variant_and_record
  $ run_tsc model/composite/variant_and_record.ts
  ../../data/ts/model/composite/variant_and_record.ts(1,10): error TS2304: Cannot find name 'movement'.

model/polymorphic/annotated_polymorphic_weird_type_name
  $ run_tsc model/polymorphic/annotated_polymorphic_weird_type_name.ts
  ../../data/ts/model/polymorphic/annotated_polymorphic_weird_type_name.ts(1,10): error TS2304: Cannot find name '_a_0'.

model/polymorphic/annotated_polymorphic
  $ run_tsc model/polymorphic/annotated_polymorphic.ts
  ../../data/ts/model/polymorphic/annotated_polymorphic.ts(1,10): error TS2304: Cannot find name 'a'.

model/polymorphic/nested_poly
  $ run_tsc model/polymorphic/nested_poly.ts
  ../../data/ts/model/polymorphic/nested_poly.ts(1,10): error TS2304: Cannot find name 'container3'.
  ../../data/ts/model/polymorphic/nested_poly.ts(1,37): error TS2304: Cannot find name 'container2'.

model/polymorphic/poly_3
  $ run_tsc model/polymorphic/poly_3.ts
  ../../data/ts/model/polymorphic/poly_3.ts(1,10): error TS2304: Cannot find name 'container3'.

model/primitive/bool_list
  $ run_tsc model/primitive/bool_list.ts

model/primitive/empty_list
  $ run_tsc model/primitive/empty_list.ts
  ../../data/ts/model/primitive/empty_list.ts(1,10): error TS2304: Cannot find name 'a'.

model/primitive/int_option_none
  $ run_tsc model/primitive/int_option_none.ts
  ../../data/ts/model/primitive/int_option_none.ts(3,17): error TS2304: Cannot find name 'a'.

model/primitive/int_option
  $ run_tsc model/primitive/int_option.ts

model/primitive/int
  $ run_tsc model/primitive/int.ts

model/primitive/LChar
  $ run_tsc model/primitive/LChar.ts
  ../../data/ts/model/primitive/LChar.ts(1,20): error TS1487: Octal escape sequences are not allowed. Use the syntax '\x00'.

model/primitive/LString
  $ run_tsc model/primitive/LString.ts

model/primitive/real
  $ run_tsc model/primitive/real.ts

model/primitive/record
  $ run_tsc model/primitive/record.ts
  ../../data/ts/model/primitive/record.ts(1,10): error TS2304: Cannot find name 'user'.

model/primitive/single_element_int_list
  $ run_tsc model/primitive/single_element_int_list.ts

model/primitive/tuple_of_bool_and_int
  $ run_tsc model/primitive/tuple_of_bool_and_int.ts

model/primitive/variant1
  $ run_tsc model/primitive/variant1.ts
  ../../data/ts/model/primitive/variant1.ts(1,10): error TS2304: Cannot find name 'status'.

model/primitive/variant2
  $ run_tsc model/primitive/variant2.ts
  ../../data/ts/model/primitive/variant2.ts(1,10): error TS2304: Cannot find name 'status'.

model/primitive/variant3
  $ run_tsc model/primitive/variant3.ts
  ../../data/ts/model/primitive/variant3.ts(1,10): error TS2304: Cannot find name 'status'.

