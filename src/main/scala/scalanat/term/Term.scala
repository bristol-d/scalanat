package scalanat.term

sealed trait Term

case class VariableTerm(variable: String) extends Term
case class ValueTerm(value: Boolean) extends Term

case class NotTerm(child: Term) extends Term

abstract class BinaryTerm(left: Term, right: Term) extends Term
case class AndTerm(left: Term, right: Term) extends BinaryTerm(left, right)
case class OrTerm(left: Term, right: Term) extends BinaryTerm(left, right)
case class ImpTerm(left: Term, right: Term) extends BinaryTerm(left, right)
