# Natural deduction in Scala

`scalanat` is an implementation of natural deduction for first-order logic in scala.
It is both a project to help students learn formal proofs in [Maths A](https://cs-uob.github.io/COMS10014) and to help me learn advanced scala techniques properly.

It contains:

  - Syntax tree definition for logical terms.
  - Parser for terms (recursive descent, done by hand to practice FP in scala).
  - Implementation of natural deduction proof rules, and parser and validator for proofs.
  - Compilation with scalajs.
  - A basic web interface.

See the unit tests for examples of how to use, or open `out/index.html` and play with the web interface (using ACE editor with custom syntax highlighting). All the proofs checked in the unit tests should work in the web editor.

## Proof language

Terms can use constants T and F, variables a-z and operators and, or, not, imp
which can also be written `&, |, ~, =>` or with the unicode characters ∧ ∨ ¬ ⇒, at least if your system supports UTF-8.

(On the windows console, with a suitable font installed, `chcp 65001` enables UTF-8.)

Proof lines must be one of 
  - `assume TERM`
  - `rule RULENAME ARGS [@ FREETERM]` where the arguments are a comma-separated list of lines to apply the rule to, and if and only if the rule expects a free term, you must provide one at the end.
  - `discharge` (applies to the latest assumption, and produces a sequent).

For example, here is a proof of the commutativity of the logical OR operation,
with the output in the comments at the end of each line:

```
assume a or b                 # 01: a ∨ b
    assume a                  # 02: a
        rule orI2 2 @ b       # 03: b ∨ a
    discharge                 # 04: a ⊢ b ∨ a
    assume b                  # 05: b
        rule orI1 5 @ a       # 06: b ∨ a
    discharge                 # 07: b ⊢ b ∨ a
    rule orE 1, 4, 7          # 08: b ∨ a
discharge                     # 09: a ∨ b ⊢ b ∨ a
```

The rule `orI2` (DisjunctionIntroduction2 in DisjunctionRules.scala) has the form `p / q ∨ p` where `q` only appears in the conclusion, so `q` is a free term. This is what lets us deduce `q ∨ p`, for any `q`, from `p`.

The rule `orE` (DisjunctionElimination) has the form `p ∨ q, p ⊢ r, q ⊢ r / r` so it needs three arguments, of which the first must be a disjunction and the other two must be sequents that match both cases of the disjunction, and have the same conclusion. This is implemented with scala pattern-matching:

```scala
case Seq(OrTerm(l, r), Sequent(Some(a1), c1), Sequent(Some(a2), c2)) =>
    if l == a1 && r == a2 && c1 == c2 then
        RuleSuccess(c1)
```

## Building

On windows, `buildweb.bat` builds and tests the whole thing. The individual steps are:

  - `sbt root/test` in the main folder runs the tests (and triggers a compile if needed).
  - `sbt rootJS/publishLocal` compiles for JS and puts the packages in the local maven repo.
  - In `scalajs`, `sbt fastLinkJS` creates the javascript files for the web.
