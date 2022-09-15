package scalanat.parser

class TokeniserTests extends munit.FunSuite:
    test("tokenising the empty string gives an empty sequence") {
        val tokens = Tokeniser("")
        assertEquals(tokens, Seq())
    }

    test("tokenising 'p and q' succeeds") {
        val tokens = Tokeniser("p and q")
        assertEquals(tokens, Seq(
            VariableToken("p"),
            OperatorToken("and"),
            VariableToken("q")
        ))
    }

    test("tokenising 'p nand q' fails") {
        val tokens = Tokeniser("p nand q")
        assertEquals(tokens, TokeniserProblem("No operator starts with 'nand'."))
    }

    test("tokenising 'p an q' fails") {
        val tokens = Tokeniser("p an q")
        assertEquals(tokens, TokeniserProblem("Did not recognise token 'an'."))
    }

    test("all operators are recognised") {
        val input = Seq(
            "and", "&", "/\\", "∧",
            "or", "|", "\\/", "∨",
            "not", "-", "~", "¬",
            "imp", "=>", "⇒",
        )
        val tokens = Tokeniser(input.mkString(" "))
        assertEquals(tokens, input map OperatorToken.apply)
    }

    test("true and false are recognised") {
        assertEquals(Tokeniser("T"), Seq(ValueToken(true)))
        assertEquals(Tokeniser("F"), Seq(ValueToken(false)))
    }

    test("a longer expression") {
        val tokens = Tokeniser("p and q or r => not F")
        assertEquals(tokens, Seq(
            VariableToken("p"),
            OperatorToken("and"),
            VariableToken("q"),
            OperatorToken("or"),
            VariableToken("r"),
            OperatorToken("=>"),
            OperatorToken("not"),
            ValueToken(false)
        ))
    }
