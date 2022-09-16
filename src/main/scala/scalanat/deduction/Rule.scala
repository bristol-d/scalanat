package scalanat.deduction

import scalanat.term.Term

/**
 * A rule is something you can apply to one or more terms or sequents
 * and returns either a new term or a way to create a new term.
 * 
 * length is the number of term (containers) expected.
 */
abstract class Rule:
    def length: Int
    def name: String
    def apply(t: (Term|Sequent)*): RuleResult

/**
 * RuleResult is a custom option type that can carry error information.
 * Some rules like (p / p ∨ q) take a "free term" once they succeed.
 * We model this with a function that applies the rule result to the free term.
 */
sealed trait RuleResult

case class RuleSuccess(t: Term) extends RuleResult
case class RuleFailure(msg: String) extends RuleResult
case class RuleSuccessFree(f: Term => Term) extends RuleResult
