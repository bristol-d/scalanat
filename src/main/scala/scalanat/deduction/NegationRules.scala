package scalanat.deduction

import scalanat.term.*

object NegationIntroduction extends Rule:
    val length = 1
    val name = "¬I"
    def apply(t: (Term|Sequent)*): RuleResult = t match {
        case Seq(Sequent(a, ValueTerm(false))) => RuleSuccess(NotTerm(a))
        case _ => RuleFailure(s"$name requires one sequent that concludes F.")
    }

object NegationElimination extends Rule:
    val length = 2
    val name = "¬E"
    def apply(t: (Term|Sequent)*): RuleResult = t match {
        case Seq(a: Term, NotTerm(b)) => 
            if a == b then RuleSuccess(ValueTerm(false))
            else RuleFailure(s"$name: second term must be negation of first.")
        case _ => RuleFailure(s"$name: requires two terms and the second must negate the first.")
    }

object LawOfExcludedMiddle extends Rule:
    val length = 0
    val name = "LEM"
    def apply(t: (Term|Sequent)*): RuleResult = t match {
        case Seq() => RuleSuccessFree(t => OrTerm(t, NotTerm(t)))
        case _ => RuleFailure(s"$name takes no parameters.")
    }

object Falsum extends Rule:
    val length = 1
    val name = "F"
    def apply(t: (Term|Sequent)*): RuleResult = t match {
        case Seq(ValueTerm(false)) => RuleSuccessFree(t => t)
        case _ => RuleFailure(s"$name: requires one term that must be F.")
    }
