package scalanat.deduction

import scalanat.term.*

/**
 * andI : p, q / p and q 
 */
object ConjunctionIntroduction extends Rule:
    val length = 2
    val name = "∧I"
    def apply(t: (Term|Sequent)*): RuleResult = t match {
        case Seq(a: Term, b: Term) =>
            RuleSuccess(AndTerm(a, b))
        case _ => RuleFailure(s"$name requires exactly two term parameters.")
    }

/**
 * andE1 : p and q / p
 */
object ConjunctionElimination1 extends Rule:
    val length = 1
    val name = "∧E1"
    def apply(t: (Term|Sequent)*): RuleResult = t match {
        case Seq(AndTerm(a, b)) =>
            RuleSuccess(a)
        case _ => RuleFailure(s"$name requires one term parameter, which must be a conjunction.")
    }

/**
 * andE2 : p and q / q
 */
object ConjunctionElimination2 extends Rule:
    val length = 1
    val name = "∧E2"
    def apply(t: (Term|Sequent)*): RuleResult = t match {
        case Seq(AndTerm(a, b)) =>
            RuleSuccess(b)
        case _ => RuleFailure(s"$name requires one term parameter, which must be a conjunction.")
    }
