package scalanat.parser

import scalanat.term.*

def tokenise(src: String): Seq[Token] =
    Tokeniser(src) match {
        case t: Seq[Token] => t
        case TokeniserProblem(m) =>
            throw RuntimeException("Tokeniser failed: " + m)
    }

class ParserTests extends munit.FunSuite:
    test("parsing 'T'") {
        val str = "T"
        val tokens = tokenise(str)
        assertEquals(Parser.parse(tokens),
            ValueTerm(true)
        )
    }

    test("parsing 'p and q'") {
        val str = "p and q"
        val tokens = tokenise(str)
        assertEquals(Parser.parse(tokens),
            AndTerm(
                VariableTerm("p"),
                VariableTerm("q")
            )
        )
    }

    test("parsing 'p and q or r'") {
        val str = "p and q or r"
        val tokens = tokenise(str)
        assertEquals(Parser.parse(tokens),
            OrTerm(
                AndTerm(
                    VariableTerm("p"),
                    VariableTerm("q")

                ),
                VariableTerm("r")
            )
        )
    }

    test("parsing 'p or q and r'") {
        val str = "p or q and r"
        val tokens = tokenise(str)
        assertEquals(Parser.parse(tokens),
            OrTerm(
                VariableTerm("p"),
                AndTerm(
                    VariableTerm("q"),
                    VariableTerm("r")
                ),
            )
        )
    }

    test("parsing '(p or q) and r'") {
        val str = "(p or q) and r"
        val tokens = tokenise(str)
        assertEquals(Parser.parse(tokens),
            AndTerm(
                OrTerm(
                    VariableTerm("p"),
                    VariableTerm("q")
                ),
                VariableTerm("r")
            )
        )
    }

    test("parsing 'p and (q or r)'") {
        val str = "p and (q or r)"
        val tokens = tokenise(str)
        assertEquals(Parser.parse(tokens),
            AndTerm(
                VariableTerm("p"),
                OrTerm(
                    VariableTerm("q"),
                    VariableTerm("r")
                ),
            )
        )
    }

    test("parsing 'not p'") {
        val str = "not p"
        val tokens = tokenise(str)
        assertEquals(Parser.parse(tokens),
            NotTerm(VariableTerm("p"))
        )
    }

    test("parsing 'not not p'") {
        val str = "not not p"
        val tokens = tokenise(str)
        assertEquals(Parser.parse(tokens),
            NotTerm(NotTerm(VariableTerm("p")))
        )
    }

    test("parsing 'not p or q'") {
        val str = "not p or q"
        val tokens = tokenise(str)
        assertEquals(Parser.parse(tokens),
            OrTerm(
                NotTerm(VariableTerm("p")),
                VariableTerm("q")
            )
        )
    }

    test("parsing 'p imp q'") {
        val str = "p imp q"
        val tokens = tokenise(str)
        assertEquals(Parser.parse(tokens),
            ImpTerm(
                VariableTerm("p"),
                VariableTerm("q")
            )
        )
    }

    test("error on trailing tokens") {
        val str = "p q"
        val tokens = tokenise(str)
        assertEquals(Parser.parse(tokens),
            ParserProblem("parse: remaining tokens at end of string, first is VariableToken(q).")
        )
    }

    test("error on trailing operator") {
        val str = "p and"
        val tokens = tokenise(str)
        assertEquals(Parser.parse(tokens),
            ParserProblem("parse: remaining tokens at end of string, first is OperatorToken(and).")
        )
    }






