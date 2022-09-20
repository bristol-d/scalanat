package scalanat.term

/**
 * A context object to choose between unicode, symbolic and English operators.
 */
trait Symbols:
    def and: String
    def or: String
    def imp: String
    def not: String
    def seq: String

object EnglishSymbols extends Symbols:
    def and = "and"
    def or = "or"
    def imp = "imp"
    def not = "not"
    def seq = "seq"

object DefaultSymbols extends Symbols:
    def and = "∧"
    def or = "∨"
    def imp = "⇒"
    def not = "¬"
    def seq = "⊢"

object AsciiSymbols extends Symbols:
    def and = "&"
    def or = "|"
    def imp = "=>"
    def not = "~"
    def seq = "#-"