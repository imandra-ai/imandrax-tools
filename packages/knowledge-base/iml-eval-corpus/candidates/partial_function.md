`List.tl` and `List.hd` are partial functions.

If they are used in verification (theorem, lemma, etc.), refuting counterexamples which falls in their undefined domain can be returned. This is confusing. 

As per prelude.iml, it's recommended to use pattern matching.

Make a rule to avoid `List.tl` and `List.hd` unconditionally (except for in comments)? They are not really adding expressiveness anyway.