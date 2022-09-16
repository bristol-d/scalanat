package scalanat.deduction

import scalanat.term.*

object ImplicationIntroduction extends Rule:
    val length = 1
    val name = "⇒I"
    def apply(t: (Term|Sequent)*): RuleResult = t match {
        case Seq(Sequent(a, b)) => RuleSuccess(ImpTerm(a, b))
        case _ => RuleFailure(s"$name: requires a sequent.")
    }

object ImplicationElimination extends Rule:
    val length = 2
    val name = "⇒E"
    def apply(t: (Term|Sequent)*): RuleResult = t match {
        case Seq(ImpTerm(a, b), c: Term) => 
            if a == c then
                RuleSuccess(b)
            else
                RuleFailure(s"$name: second term must be the assumption of the first.")
        case _ => RuleFailure(s"$name: requires an implication and a second term.")
    }
