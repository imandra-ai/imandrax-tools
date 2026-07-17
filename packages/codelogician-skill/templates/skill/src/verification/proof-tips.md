---
name: proof-tips
description: Practical tips for writing proofs in IML.
---

- The auto tactic, `auto` as in `[@@by auto]`, is ImandraX's automated inductive waterfall proof strategy, which combines simplification and may decide to do induction. This is the most common way to prove a `theorem` in ImandraX automatically.
  - Simplification is in many ways the most important part of the waterfall, and the step that most often causes a clause to evaporate or the goal to be refuted
  - For this reason, making good use of rewrite rules (`[@@rw]`) in order to control simplification is perhaps the most powerful tool ImandraX gives us. Thus it's important to spend as much time as possible teaching ImandraX a good set of rules to apply.
- Do small proofs
    - Do proofs in small steps. If you want to sketch out a bunch of ideas, comment out the parts that is not yet ready to be proved.
    - When you see `ImandraX internal error`, bisect the problem by commenting out parts of the code.
- Avoid inline lambdas in proofs. Use named functions instead.
- Automated tactics are not a silver bullet. They (e.g. `auto`) might not be suitable for every tasks. Resort to manual tactics for surgical proof control.
- Sometimes `auto` can give confusing goal states.
- It's a strongly discouraged anti-pattern to attemp trial-and-error on tactics. Permulating tactics one-by-one is the most likely way to get yourself into a vicious cycle. You will see sligthly different goal states every time but you won't make any progress. When this happens, it's better to reflect and lay out your thoughts and plans in prose first. Track what works and what is dead ends somewhere, revise your plan as you go along.
