package scalanat.deduction

import scalanat.term.Term

/**
 * A rule is something you can apply to one or more term containers
 * (containers are needed since discharged assumptions are not terms)
 * and returns a new term.
 * 
 * length is the number of term (containers) expected.
 */
abstract class Rule:
    def length: Int
    def name: String
    def apply(t: (Term|Discharge)*): RuleResult

/**
 * RuleResult is a custom option type that can carry error information.
 */
sealed trait RuleResult

case class RuleSuccess(t: Term) extends RuleResult
case class RuleFailure(msg: String) extends RuleResult


