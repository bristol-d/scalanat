package scalanat.deduction

import scalanat.term.*

object ConjunctionIntroduction extends Rule:
    val length = 2
    val name = "∧I"
    def apply(t: (Term|Discharge)*): RuleResult = t match {
        case Seq(a: Term, b: Term) =>
            RuleSuccess(AndTerm(a, b))
        case _ => RuleFailure(s"$name requires exactly two term parameters.")
    }

object ConjunctionElimination1 extends Rule:
    val length = 2
    val name = "∧E1"
    def apply(t: (Term|Discharge)*): RuleResult = t match {
        case Seq(AndTerm(a, b)) =>
            RuleSuccess(a)
        case _ => RuleFailure(s"$name requires one term parameter, which must be a conjunction.")
    }

object ConjunctionElimination2 extends Rule:
    val length = 2
    val name = "∧E2"
    def apply(t: (Term|Discharge)*): RuleResult = t match {
        case Seq(AndTerm(a, b)) =>
            RuleSuccess(b)
        case _ => RuleFailure(s"$name requires one term parameter, which must be a conjunction.")
    }
