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
        assertEquals(Parser(tokens),
            ValueTerm(true)
        )
    }

    test("parsing 'p and q'") {
        val str = "p and q"
        val tokens = tokenise(str)
        assertEquals(Parser(tokens),
            AndTerm(
                VariableTerm("p"),
                VariableTerm("q")
            )
        )
    }

    test("parsing 'p and q or r'") {
        val str = "p and q or r"
        val tokens = tokenise(str)
        assertEquals(Parser(tokens),
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
        assertEquals(Parser(tokens),
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
        assertEquals(Parser(tokens),
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
        assertEquals(Parser(tokens),
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
        assertEquals(Parser(tokens),
            NotTerm(VariableTerm("p"))
        )
    }

    test("parsing 'not not p'") {
        val str = "not not p"
        val tokens = tokenise(str)
        assertEquals(Parser(tokens),
            NotTerm(NotTerm(VariableTerm("p")))
        )
    }

    test("parsing 'not p or q'") {
        val str = "not p or q"
        val tokens = tokenise(str)
        assertEquals(Parser(tokens),
            OrTerm(
                NotTerm(VariableTerm("p")),
                VariableTerm("q")
            )
        )
    }





