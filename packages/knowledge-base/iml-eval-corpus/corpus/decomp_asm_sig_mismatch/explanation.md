Region decomposition (`[@@decomp top ~assuming:[%id ...]]`) fails internally when the `~assuming` predicate's argument type does not match the type of the function being decomposed. The failure surfaces as a `TacticEvalErr` mentioning `Imandrax_decomp__Strategy.Inject_asm`, with no proof-obligation results produced.

Fix: ensure the `~assuming` precondition takes an argument of the same type as the target function. For example, rewrite a `real`-typed precondition to use integer arithmetic matching an `int`-typed target.
