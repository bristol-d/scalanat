package scalanat.deduction

import scalanat.term.*

class ImplicationRuleTests extends munit.FunSuite:

    test("introducing an implication") {
        assertEquals(
            ImplicationIntroduction(Sequent(
                Some(VariableTerm("p")),
                VariableTerm("q")
            )),
            RuleSuccess(
                ImpTerm(
                    VariableTerm("p"),
                    VariableTerm("q")
                )
            )
        )
    }

    test("introducing an implication fails") {
        assertEquals(
            ImplicationIntroduction(
                VariableTerm("p")
            ),
            RuleFailure("→I: requires a sequent.")
        )
    }

    test("eliminating implication (modus ponens)") {
        assertEquals(
            ImplicationElimination(
                ImpTerm(
                    VariableTerm("p"),
                    OrTerm(
                        VariableTerm("q"),
                        VariableTerm("r")
                    )
                ),
                VariableTerm("p")
            ),
            RuleSuccess(
                OrTerm(
                    VariableTerm("q"),
                    VariableTerm("r")
                )
            )
        )
    }

    test("eliminating implication fails") {
        assertEquals(
            ImplicationElimination(
                ImpTerm(
                    VariableTerm("p"),
                    OrTerm(
                        VariableTerm("q"),
                        VariableTerm("r")
                    )
                ),
                VariableTerm("q")
            ),
            RuleFailure("→E: second term must be the assumption of the first.")
        )

        assertEquals(
            ImplicationElimination(
                AndTerm(
                    VariableTerm("p"),
                    OrTerm(
                        VariableTerm("q"),
                        VariableTerm("r")
                    )
                ),
                VariableTerm("p")
            ),
            RuleFailure("→E: requires an implication and a second term.")
        )
    }
