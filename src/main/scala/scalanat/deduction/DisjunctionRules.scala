package scalanat.deduction

import scalanat.term.*

object DisjunctionIntroduction1 extends Rule:
    val length = 1
    val name = "∨I1"
    def apply(t: (Term|Sequent)*): RuleResult = t match {
        case Seq(a: Term) =>
            RuleSuccessFree(t => OrTerm(a, t))
        case _ => RuleFailure(s"$name requires exactly one term parameter.")
    }

object DisjunctionIntroduction2 extends Rule:
    val length = 1
    val name = "∨I2"
    def apply(t: (Term|Sequent)*): RuleResult = t match {
        case Seq(a: Term) =>
            RuleSuccessFree(t => OrTerm(t, a))
        case _ => RuleFailure(s"$name requires exactly one term parameter.")
    }

object DisjunctionElimination extends Rule:
    val length = 3
    val name = "∨E"
    def apply(t: (Term|Sequent)*): RuleResult = t match {
        case Seq(OrTerm(l, r), Sequent(Some(a1), c1), Sequent(Some(a2), c2)) =>
            if l == a1 && r == a2 && c1 == c2 then
                RuleSuccess(c1)
            else
                RuleFailure(s"$name failed to pattern match. Need (p ∨ q, p ⊢ r, q ⊢ r) to derive r.")
        case _ => RuleFailure(s"$name requires one disjunction term and two sequents.")
    }