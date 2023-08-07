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
            "not", "~", "¬",
            "imp", "->", "→",
        )
        val tokens = Tokeniser(input.mkString(" "))
        assertEquals(tokens, input map OperatorToken.apply)
    }

    test("true and false are recognised") {
        assertEquals(Tokeniser("T"), Seq(ValueToken(true)))
        assertEquals(Tokeniser("F"), Seq(ValueToken(false)))
    }

    test("a longer expression") {
        val tokens = Tokeniser("p and q or r -> not F")
        assertEquals(tokens, Seq(
            VariableToken("p"),
            OperatorToken("and"),
            VariableToken("q"),
            OperatorToken("or"),
            VariableToken("r"),
            OperatorToken("->"),
            OperatorToken("not"),
            ValueToken(false)
        ))
    }

    test ("brackets without spaces") {
        val tokens = Tokeniser("p/\\(q\\/r)")
        assertEquals(tokens, Seq(
            VariableToken("p"),
            OperatorToken("/\\"),
            OpenBracketToken(),
            VariableToken("q"),
            OperatorToken("\\/"),
            VariableToken("r"),
            CloseBracketToken()
        ))
    }

    test ("brackets with spaces") {
        val tokens = Tokeniser("p /\\ ( q \\/ r )")
        assertEquals(tokens, Seq(
            VariableToken("p"),
            OperatorToken("/\\"),
            OpenBracketToken(),
            VariableToken("q"),
            OperatorToken("\\/"),
            VariableToken("r"),
            CloseBracketToken()
        ))
    }

    test ("nested brackets") {
        val tokens = Tokeniser("not((a|b)&c)")
        assertEquals(tokens, Seq(
            OperatorToken("not"),
            OpenBracketToken(),
            OpenBracketToken(),
            VariableToken("a"),
            OperatorToken("|"),
            VariableToken("b"),
            CloseBracketToken(),
            OperatorToken("&"),
            VariableToken("c"),
            CloseBracketToken()
        ))
    }

    test ("nested brackets with spacing") {
        val tokens = Tokeniser(" not ( ( a | b ) & c ) ")
        assertEquals(tokens, Seq(
            OperatorToken("not"),
            OpenBracketToken(),
            OpenBracketToken(),
            VariableToken("a"),
            OperatorToken("|"),
            VariableToken("b"),
            CloseBracketToken(),
            OperatorToken("&"),
            VariableToken("c"),
            CloseBracketToken()
        ))
    }

    test("tokens starting with T/F") {
        assertEquals(Tokeniser("T -> F"), Seq(
            ValueToken(true),
            OperatorToken("->"),
            ValueToken(false),
        ))
        assertEquals(Tokeniser("T->F"), Seq(
            ValueToken(true),
            OperatorToken("->"),
            ValueToken(false),
        ))
        assertEquals(Tokeniser("Ta"), TokeniserProblem("At end of string, did not recognise token 'Ta'."))
    }

