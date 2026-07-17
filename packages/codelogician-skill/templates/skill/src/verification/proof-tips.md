---
name: proof-tips
description: Practical tips for writing proofs in IML.
---

# Factual Notes

- When there's no tactic attached to a definition, `unroll` is used by default.

# Tips

- The auto tactic, `auto` as in `[@@by auto]`, is ImandraX's automated inductive waterfall proof strategy, which combines simplification and may decide to do induction. This is the most common way to prove a `theorem` in ImandraX automatically.
  - Simplification is in many ways the most important part of the waterfall, and the step that most often causes a clause to evaporate or the goal to be refuted
  - For this reason, making good use of rewrite rules (`[@@rw]`) in order to control simplification is perhaps the most powerful tool ImandraX gives us. Thus it's important to spend as much time as possible teaching ImandraX a good set of rules to apply.
- Do small proofs
    - Do proofs in small steps. If you want to sketch out a bunch of ideas, comment out the parts that is not yet ready to be proved.
    - When you see `ImandraX internal error`, bisect the problem by commenting out parts of the code.
- Avoid inline lambdas in proofs. Use named functions instead.
- Automated tactics are not a silver bullet. They (e.g. `auto`) might not be suitable for every tasks. Resort to manual tactics for surgical proof control.
- Sometimes `auto` can give confusing goal states.
- Do not permulate tactics: it's a strongly discouraged anti-pattern to attemp trial-and-error on tactics. Permulating tactics one-by-one is the most likely way to get yourself into a vicious cycle. You will see sligthly different goal states every time but you won't make any progress. When this happens, it's better to reflect and lay out your thoughts and plans in prose first. Track what works and what is dead ends somewhere, revise your plan as you go along.
- Choose tactics wisely: we have a variety of tactics to choose from. Some are more suitable for certain tasks than others. See [tactics reference](./../reference/tactics.md) for more details.
- Tips for writing verification-friendly code:
    - Avoid higher order functions if you can! E.g. `List.map`, `List.filter`, `List.fold_left`, and etc.
        - It is sometimes better to roll your own function rather than using higher order functions like map, fold, etc.
    - Avoid recursive functions with accumulators, if necessary use fold-right.
    - One recursion per function. No nested recursion.: they trigger nested proof-obligation about termination proving. tricky.
    - Do not use mutually recursive functions if you can.
    - If you want to match against it, you should name it!
    - If you know the types of how a function will be used, go ahead and fix them (context dependent, maybe not necessary).
    - Avoid lambdas in the pattern of a rule.
    - Patterns are used to apply them to terms in a proof.  If a rule's pattern has a lambda, it makes it difficult to match against terms.
    - Sometimes, if an obvious proof is failing, you may be trying to prove something too specific!
    - Try proving something a little stronger, with another variable, and you may be surprised.
    - You can sometimes tell if you are in this situation because the induction gets stuck at a point where you have no hypothesis.
    - If having trouble proving things involving an accumulator, you may need many lemmas about it.
    - Sometimes it is better to reason directly with the function using an accumulator, wrather than reasoning on a function that starts with accumulator 0.
    - Similarly, it can be better to use the function with the accumulator in your measures, rather than wrapping it.
    - Encode properties as functions rather than as properties of data structures.
    - The more that is there "by law" the less you have to derive.
    - Primitive concepts for each concept.
    - If you are reasoning by enforcing predicates on some type, you are likely to need generalization rules!