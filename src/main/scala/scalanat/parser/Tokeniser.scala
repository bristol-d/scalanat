package scalanat.parser

import scala.annotation.tailrec

/**
 * We restrict the variables to lowercase a-z
 * and allow all the following operators:
 * and & /\ ∧
 * or  | \/ ∨
 * not - ~ ¬
 * imp => ⇒
 * T F
 */
sealed trait Token

case class VariableToken(variable: String) extends Token
case class ValueToken(value: Boolean) extends Token
case class OperatorToken(operator: String) extends Token

case class TokeniserProblem(message: String)

object Tokeniser:
    val OneCharOps = Set('&', '|', '∧', '∨', '-', '~', '¬', '⇒')
    val Words = Set("and", "or", "not", "imp")
    val Symbols = Set("/\\", "\\/", "=>")

    type State = (Seq[Token], Seq[Char])
    enum Scan:
        case Start // no pending token
        case ABuffer(s: String) // inside a token that starts with an alphanum character
        case BBuffer(s: String) // inside a token that starts with a non-alphanum character

    def is_letter(c: Char): Boolean = c >= 'a' && c <= 'z'
    def is_alpha(c: Char): Boolean = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    
    /**
     * Does this sequence not start with an alphabet character?
     * Whether because it's empty, or because it starts with something else.
     * Used when we've just read a T/F value, for example.
     */
    def not_alpha(s: Seq[Char]): Boolean = s match {
        case Seq() => true
        case h +: t if !is_alpha(h) => true
        case _ => false
    }

    def apply(src: String): Seq[Token] | TokeniserProblem =
        tokenise((Seq(), src.toCharArray().toSeq), Scan.Start)

    // The textbook way to do this would be to have two mutually recursive functions for the two cases,
    // but Scala isn't guaranteed to optimise that, so they're folded into one.
    @tailrec 
    def tokenise(in: State, scan: Scan): Seq[Token] | TokeniserProblem =
        val (acc, src) = in
        scan match {
            case Scan.Start => src match {
                // in this case, it's ok to strip a sequence of whitespace
                case ' ' +: rest => tokenise((acc, rest), Scan.Start)

                // variable: single letter followed by something non-letter
                case a +: rest if is_letter(a) && not_alpha(rest) => tokenise((VariableToken(a.toString()) +: acc, rest), Scan.Start)

                // T, F
                case 'T' +: rest if not_alpha(rest) => tokenise((ValueToken(true) +: acc, rest), Scan.Start)
                case 'F' +: rest if not_alpha(rest) => tokenise((ValueToken(false) +: acc, rest), Scan.Start)

                // empty sequence
                case Seq() => acc.reverse // we're done!

                // one-symbol operators
                case a +: rest if OneCharOps.contains(a) => tokenise((OperatorToken(a.toString()) +: acc, rest), Scan.Start)

                // start of something else alphabetic
                case a +: rest if is_alpha(a) => tokenise((acc, rest), Scan.ABuffer(a.toString()))

                // start of something else non-alphabetic
                case a +: rest => tokenise((acc, rest), Scan.BBuffer(a.toString()))

                // note: this match is exhaustive
            }
            case Scan.ABuffer(s) => src match {
                case Seq() => if Words.contains(s) 
                              then (OperatorToken(s.toString()) +: acc) 
                              else TokeniserProblem(s"At end of string, did not recognise token '$s'.")
                case a +: rest if s.length() <= 3 && is_letter(a) => tokenise((acc, rest), Scan.ABuffer(s + a))
                case a +: rest if s.length() > 3 => TokeniserProblem(s"No operator starts with '$s'.")

                // if we get here, we've found something that's not part of our word.
                case a +: rest => if Words.contains(s) 
                                  then tokenise((OperatorToken(s.toString()) +: acc, a +: rest), Scan.Start) 
                                  else TokeniserProblem(s"Did not recognise token '$s'.")
            }
            case Scan.BBuffer(s) => src match {
                case Seq() => if Symbols.contains(s) 
                              then (OperatorToken(s.toString()) +: acc) 
                              else TokeniserProblem(s"At end of string, did not recognise token '$s'.")
                case a +: rest if s.length() <= 2 && !is_letter(a) && a != ' ' => tokenise((acc, rest), Scan.BBuffer(s + a))
                case a +: rest if s.length() > 2 => TokeniserProblem(s"No operator starts with '$s'.")

                // if we get here, we've found something that's not part of our operator.
                case a +: rest => if Symbols.contains(s) 
                                  then tokenise((OperatorToken(s.toString()) +: acc, a +: rest), Scan.Start) 
                                  else TokeniserProblem(s"Did not recognise token '$s'.")
            }
        }
