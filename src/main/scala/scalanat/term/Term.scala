package scalanat.term

sealed abstract class Term:
    def out(using symbols: Symbols): String

case class VariableTerm(val variable: String) extends Term:
    def out(using symbols: Symbols): String = variable

case class ValueTerm(val value: Boolean) extends Term:
    def out(using symbols: Symbols): String = value match {
        case true => "T"
        case false => "F"
    }

case class NotTerm(child: Term) extends Term:
    def out(using symbols: Symbols): String = child match {
        // why can't we use BinaryTerm here ???
        case AndTerm(_, _) | OrTerm(_, _) | ImpTerm(_, _) => s"${symbols.not}(${child.out})"
        case _ => s"${symbols.not}${child.out}"
    }

abstract class BinaryTerm(left: Term, right: Term) extends Term

/**
 * This exists so we don't have to instantiate via generics.
 */
object BinaryTerms:
    def apply(op: String, left: Term, right: Term): Term =
        op match {
            case "and" => AndTerm(left, right)
            case "or" => OrTerm(left, right)
            case "imp" => ImpTerm(left, right)
            case _ => throw RuntimeException("Internal BinaryTerm.apply called on incorrect type.")
        }

case class AndTerm(left: Term, right: Term) extends BinaryTerm(left, right):
    def out(using symbols: Symbols): String = 
        val ls = left match {
            case AndTerm(_, _) => s"(${left.out})"
            case _ => left.out
        }
        val rs = right match {
            case AndTerm(_, _) => s"(${right.out})"
            case _ => right.out
        }
        s"$ls ${symbols.and} $rs"

case class OrTerm(left: Term, right: Term) extends BinaryTerm(left, right):
    def out(using symbols: Symbols): String = 
        val ls = left match {
            case AndTerm(_, _) | OrTerm(_, _) => s"(${left.out})"
            case _ => left.out
        }
        val rs = right match {
            case AndTerm(_, _) | OrTerm(_, _) => s"(${right.out})"
            case _ => right.out
        }
        s"$ls ${symbols.or} $rs"

case class ImpTerm(left: Term, right: Term) extends BinaryTerm(left, right):
    def out(using symbols: Symbols): String = 
        val ls = left match {
            case AndTerm(_, _) | OrTerm(_, _) | ImpTerm(_, _) => s"(${left.out})"
            case _ => left.out
        }
        val rs = right match {
            case AndTerm(_, _) | OrTerm(_, _) | ImpTerm(_, _)  => s"(${right.out})"
            case _ => right.out
        }
        s"$ls ${symbols.imp} $rs"
