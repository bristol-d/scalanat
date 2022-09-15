package scalanat.term

sealed class Term

case class VariableTerm(variable: String) extends Term
case class ValueTerm(value: Boolean) extends Term

case class NotTerm(child: Term) extends Term

abstract class BinaryTerm(left: Term, right: Term) extends Term

/**
 * This exists so we don't have to instantiate via generics.
 */
object BinaryTerm:
    def apply(op: String, left: Term, right: Term): Term =
        op match {
            case "and" => AndTerm(left, right)
            case "or" => OrTerm(left, right)
            case "imp" => ImpTerm(left, right)
            case _ => throw RuntimeException("Internal BinaryTerm.apply called on incorrect type.")
        }

case class AndTerm(left: Term, right: Term) extends BinaryTerm(left, right)
case class OrTerm(left: Term, right: Term) extends BinaryTerm(left, right)
case class ImpTerm(left: Term, right: Term) extends BinaryTerm(left, right)
