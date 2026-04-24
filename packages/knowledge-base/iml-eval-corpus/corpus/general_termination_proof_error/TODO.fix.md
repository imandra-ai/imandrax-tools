for t2.iml and t3.iml, the message is
"The default termination measure for recursive function `g` is counter-satisfiable. Supply a custom `[@@measure ...]` returning `Ordinal.t` that strictly decreases on every recursive call."

- "g" is incorrect because it's `f` that has termination proof error.
- "default termination measure" is wrong because t2.iml has a measure attribute.
- root cause:
   - in eval results, we have no way to know which function is the one with termination proof error.
   - there exists one PO result that has `.origin.from_sym == "f"` but it's not the one that has termination proof error. The error one has `.origin == null`.