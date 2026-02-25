## Core Concepts

- Decomposition Goal: To break down a function f into a set of regions [ {cs_0, inv_0}, {cs_1, inv_1}, ..., {cs_m, inv_m} ] such that the function’s body is equivalent to if cs_0 then inv_0 else if cs_1 then inv_1 ... else inv_m over the relevant input domain.
- Regions: Each region consists of:
  - Constraints (cs): A list of Boolean terms representing conditions on the function’s inputs. Logically, these constraints within a single region form a conjunction (AND). The constraint language avoids complex control flow like ITE or match expressions.
  - Invariant (inv): The simplified symbolic expression representing the function’s output when the region’s constraints are satisfied.
- Disjointness & Coverage: For a decomposition generated directly by top (without ~assuming), the regions are typically disjoint (an input satisfies the constraints of at most one region), and together they cover the function’s entire input domain. However, subsequent manipulation using composition operators (like <<, ~|, etc.) might result in overlapping regions. Furthermore, using the ~assuming option restricts the analysis, meaning the regions only cover the portion of the input domain where the assumption holds. The decomposition aims to represent the covered domain’s behavior in a DNF-like structure based on its input conditions.
- Feasibility & Models: The constraints of each region can be checked for satisfiability. If feasible, a concrete model (an example input satisfying the constraints) can often be extracted.


## Basic Usage

The top function from the Decomp module is primary way to initiate decomposition, it starts the process and accepts several optional configuration arguments:

    Basic Invocation:

      let f x = if x = 1 || x = 2 then x + 1 else x - 1
      [@@decomp top ()]
      (* Produces regions based on conditions x=1, x=2, etc. *)

    ~assuming: Decomposes the function under an additional assumption (a precondition). Takes an identifier representing a Boolean function.

      let asm x = x > 1
      let g x = ...
      [@@decomp top ~assuming:[%id asm] ()]
      (* Decomposes g only for inputs where asm(x) is true *)

    ~basis: Specifies a list of function identifiers that should not be expanded or interpreted during decomposition. They remain symbolic in the resulting regions.

      let g x = if f x > x then -1 else 1
      [@@decomp top ~basis:[ [%id f] ] ()]
      (* Decomposes g, but leaves calls to f as 'f x' *)

    ~ctx_simp: Enables/disables contextual simplification during decomposition (e.g., value propagation, rewriting x <> y as not (x = y)).

      let i x y = ...
      [@@decomp top ~ctx_simp:true ()]

    ~prune: Boolean flag to attempt full pruning of infeasible regions and model extraction for the feasible regions during the initial top step.
    ~rule_specs: Specifies identifiers of rule specs to be used for rewriting or forward-chaining within the decomposition process.

## Common Errors

**Mismatched ~assuming signature:**
```iml
(* WRONG - is_valid has 1 param, process has 2 *)
let is_valid x = x > 0
let process x y = x + y
[@@decomp top ~assuming:[%id is_valid] ()]

(* CORRECT - same parameters *)
let is_valid x y = x > 0 && y > 0
let process x y = x + y
[@@decomp top ~assuming:[%id is_valid] ()]
```

## Advanced features
### Composition Operators

The [@@decomp ...] syntax allows chaining operations using operators defined in the Decomp module to build more complex analysis pipelines:

    m |>> f (Apply Refiner): Takes the result of a decomposition m (e.g., from top ()) and applies a refiner function f to it. This is the main way to chain steps like pruning or enumeration.

      let f' = f [@@decomp top () |>> prune]

    m << d1 (Merge): Merges the decomposition d1 into m. This effectively substitutes the definition of the function decomposed in d1 wherever it appears in the regions of m, re-calculating regions accordingly.

      (* Concept: Decompose g, then merge f's definition into it *)
      let d_g_merged_f = g [@@decomp top ~basis:[ [%id f] ] () << top () [%id f ]]

    m <|< d1 (Compound Merge): A specialized merge that combines merging (<<) and combining (~|, see below) but preserves certain region distinctions from m.

      let d_g_compound_f = g [@@decomp top ~basis:[ [%id f] ] () <|< top () [%id f ]]

    ~| m (Combine): Takes a decomposition m and merges regions that have the exact same invariant, creating a disjunction (OR) of their constraints.

      let d_g_combined = ... [@@decomp ~| (top ~basis:[ [%id f] ] ())]

### Key Refiners (Used with |>>)

These functions from the Decomp module modify an existing decomposition:

    prune: Attempts to identify and remove infeasible regions (those whose constraints are unsatisfiable).

      let h a = ...
      let d_h_pruned = h [@@decomp top () |>> prune ]

    enumerate / enumerate_all: Refiners used for specific enumeration tasks, potentially related to generating test data satisfying region constraints. enumerate_all takes a list of decompositions and a style (Additive or Multiplicative).

      let en' = en [@@decomp top () |>> enumerate_all ~style:Additive [ ... ]]
      let en'' = en [@@decomp top () |>> enumerate_all ~style:Multiplicative [ ... ]]

    merge_ / compound_merge_: The underlying refiners used by the << and <|< operators respectively.

Rule Specifications (~rule_specs)

You can provide background axioms or lemmas (marked [@@imandra_rule_spec]) via the ~rule_specs argument to top. These rules are applied during decomposition for:

    Rewriting: Simplifying terms within constraints or invariants.
    Forward Chaining: Adding derived facts to a region’s context, potentially aiding feasibility checking or further simplification.

      let map_recons len f x = ... [@@rw] [@@imandra_rule_spec]
      let map_cons_eval f x y = ... [@@rw] [@@imandra_rule_spec]

      let d_tgt''_rs = tgt''
        [@@decomp top ~assuming:[%id asm'']
                        ~rule_specs:[ [%id map_recons]; [%id map_cons_eval] ] ()]
