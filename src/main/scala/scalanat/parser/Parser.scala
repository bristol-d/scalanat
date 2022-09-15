package scalanat.parser

import scalanat.term.*

case class ParserProblem(message: String)

case class InvalidOperatorException(message: String) extends RuntimeException(message)

object Parser:
    def apply(input: Seq[Token]): Term | ParserProblem =
        parse(input)

    def parse(input: Seq[Token]): Term | ParserProblem =
        val tokens = input map adjustToken
        val (out, rest) = parse_term(tokens)
        out

    type parseR = (Term|ParserProblem, Seq[Token])
    type parseF = Seq[Token] => parseR

    /**
     * Take the tail if we can, but if not, just return an empty
     * sequence rather than crashing, as someone else will deal
     * with this case for us.
     */
    def safetail(tokens: Seq[Token]): Seq[Token] =
        tokens match {
            case Seq() => Seq()
            case h +: t => t
        }

    /**
     * Parse a right-recursion of the format
     * INIT [op F]*
     * where INIT has already been parsed.
     */
    def parse_repeat(init: Term, op: String, f: parseF, tokens: Seq[Token]): parseR =
        var term = init
        var rest = tokens
        while rest.length > 0 && rest.head == OperatorToken(op) do
            val (t, r) = f(safetail(rest))
            t match {
                case ParserProblem(_) => return (t, rest)
                case tt: Term => 
                    term = BinaryTerm(op, term, tt)
                    rest = r
            }
        (term, rest)

    /**
     * TERM := TERM1 [or TERM1]*
     */
    def parse_term(tokens: Seq[Token]): parseR =
        val (t, rest) = parse_term1(tokens)
        t match {
            case ParserProblem(_) => (t, rest)
            case tt: Term => parse_repeat(tt, "or", parse_term1, rest)
        }

    /**
     * TERM1 := TERM2 [and TERM3]* 
     */
    def parse_term1(tokens: Seq[Token]): parseR =
        val (t, rest) = parse_term2(tokens)
        t match {
            case ParserProblem(_) => (t, rest)
            case tt: Term => parse_repeat(tt, "and", parse_term2, rest)
        }

    /**
     * TERM2 := not TERM2 | TERM3 
     */
    def parse_term2(tokens: Seq[Token]): parseR =
        if tokens.length > 0 && tokens(0) == OperatorToken("not") then
            val (t, r) = parse_term2(safetail(tokens))
            t match {
                case ParserProblem(_) => return (t, r)
                case tt: Term => (NotTerm(tt), r)
            }
        else
            parse_term3(tokens)
    
    /**
     * TERM3 := ( TERM ) | VALUE | VARIABLE
     */
    def parse_term3(tokens: Seq[Token]): parseR =
        tokens match {
            case (h +: t) =>
                h match {
                        case OperatorToken(op) => (ParserProblem(s"term3: expected variable or value, found keyword '$op'."), tokens)
                        case ValueToken(v) => (ValueTerm(v), t)
                        case VariableToken(v) => (VariableTerm(v), t)
                        case OpenBracketToken() =>
                            parse_term(t) match {
                                case (p @ ParserProblem(_), r) => (p, r)
                                case (t: Term, CloseBracketToken() +: rr) => (t, rr)
                                case _ => (ParserProblem("term3: Failed to find a ')' after a '('."), tokens)
                            }
                        case CloseBracketToken() =>
                            (ParserProblem("term3: Closing bracket with no earlier opening bracket."), tokens)
                }
            case Seq() => (ParserProblem("term3: end of token stream"), tokens)
        }

    val OPERATORS = Map(
        "and" -> "and",
        "&" -> "and",
        "/\\" -> "and",
        "∧" -> "and",
        "or" -> "or",
        "|" -> "or",
        "\\/" -> "or",
        "∨" -> "or",
        "not" -> "not",
        "-" -> "not",
        "~" -> "not",
        "¬" -> "not",
        "imp" -> "imp",
        "=>" -> "imp",
        "⇒" -> "imp"
    )

    def adjustToken(t: Token): Token = t match {
        case OperatorToken(s) => if OPERATORS.contains(s) 
                                 then OperatorToken(OPERATORS(s))
                                 else throw InvalidOperatorException(s)
        case _ => t
    }
