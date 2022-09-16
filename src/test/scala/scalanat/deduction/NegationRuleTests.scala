package scalanat.deduction

import scalanat.term.*

class NegationRuleTests extends munit.FunSuite:

    test("introducing a negation") {
        assertEquals(
            NegationIntroduction(
                Sequent(
                    VariableTerm("p"),
                    ValueTerm(false)
                )
            ),
            RuleSuccess(
                NotTerm(
                    VariableTerm("p")
                )
            )
        )
    }

    test("introducing a negation failse") {
        assertEquals(
            NegationIntroduction(
                Sequent(
                    VariableTerm("p"),
                    ValueTerm(true)
                )
            ),
            RuleFailure("¬I requires one sequent that concludes F.")
        )

        assertEquals(
            NegationIntroduction(
                VariableTerm("p")
            ),
            RuleFailure("¬I requires one sequent that concludes F.")
        )
    }

    test("eliminating a negation") {
        assertEquals(
            NegationElimination(
                VariableTerm("a"),
                NotTerm(VariableTerm("a"))
            ),
            RuleSuccess(ValueTerm(false))
        )
    }

    test("eliminating a negation fails") {
        assertEquals(
            NegationElimination(
                VariableTerm("a"),
                NotTerm(VariableTerm("b"))
            ),
            RuleFailure("¬E: second term must be negation of first.")
        )

        assertEquals(
            NegationElimination(
                VariableTerm("a"),
                VariableTerm("a")
            ),
            RuleFailure("¬E: requires two terms and the second must negate the first.")
        )
    }

    test("law of excluded middle") {
        LawOfExcludedMiddle() match {
            case RuleSuccessFree(f) =>
                assertEquals(f(ValueTerm(true)),
                    OrTerm(
                        ValueTerm(true),
                        NotTerm(ValueTerm(true))
                    )
                )
                assertEquals(f(VariableTerm("a")),
                    OrTerm(
                        VariableTerm("a"),
                        NotTerm(VariableTerm("a"))
                    )
                )
            case _ => assert(false, "Expected a free term.")
        }
    }

    test("falsum") {
        Falsum(ValueTerm(false)) match {
            case RuleSuccessFree(f) =>
                assertEquals(f(ValueTerm(true)), ValueTerm(true))
                assertEquals(f(VariableTerm("a")), VariableTerm("a"))
            case _ => assert(false, "Expected a free term.")
        }
    }

    test("falsum fails") {
        assertEquals(
            Falsum(ValueTerm(true)),
            RuleFailure("F: requires one term that must be F.")
        )

        assertEquals(
            Falsum(VariableTerm("a")),
            RuleFailure("F: requires one term that must be F.")
        )
    }
