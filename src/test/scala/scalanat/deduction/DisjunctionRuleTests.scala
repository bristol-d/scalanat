package scalanat.deduction

import scalanat.term.*

class DisjunctionRuleTests extends munit.FunSuite:

    val ta = VariableTerm("a")
    val tb = VariableTerm("b")

    test("introducing a disjunction on the right") {
        DisjunctionIntroduction1(ta) match {
            case RuleSuccessFree(f) =>
                assertEquals(f(tb), OrTerm(ta, tb))
            case _ => assert(false, "Expected success with a free term.")
        }
    }

    test("introducing a disjunction on the left") {
        DisjunctionIntroduction2(ta) match {
            case RuleSuccessFree(f) =>
                assertEquals(f(tb), OrTerm(tb, ta))
            case _ => assert(false, "Expected success with a free term.")
        }
    }

    test("disjunction introduction on empty list fails") {
        DisjunctionIntroduction1() match {
            case RuleFailure(_) => ()
            case _ => assert(false, "Expected to fail as no term.")
        }
    }

    test("disjunction introduction on list of two items fails") {
        DisjunctionIntroduction1(ta, tb) match {
            case RuleFailure(_) => ()
            case _ => assert(false, "Expected to fail as two terms.")
        }
    }

    test ("eliminating a disjunction") {
        assertEquals(
            DisjunctionElimination(
                OrTerm(VariableTerm("a"), VariableTerm("b")),
                Sequent(VariableTerm("a"), VariableTerm("x")),
                Sequent(VariableTerm("b"), VariableTerm("x"))
            ),
            RuleSuccess(VariableTerm("x"))
        )
    }

    test ("failing to eliminate a disjunction") {
        val e1 = "∨E failed to pattern match. Need (p ∨ q, p ⊢ r, q ⊢ r) to derive r."
        val e2 = "∨E requires one disjunction term and two sequents."

        assertEquals(
            DisjunctionElimination(
                // wrong sequent
                OrTerm(VariableTerm("a"), VariableTerm("b")),
                Sequent(VariableTerm("a"), VariableTerm("x")),
                Sequent(VariableTerm("c"), VariableTerm("x"))
            ), RuleFailure(e1)
        )

        assertEquals(
            DisjunctionElimination(
                // sequents back to front
                OrTerm(VariableTerm("a"), VariableTerm("b")),
                Sequent(VariableTerm("b"), VariableTerm("x")),
                Sequent(VariableTerm("a"), VariableTerm("x"))
            ), RuleFailure(e1)
        )

        assertEquals(
            DisjunctionElimination(
                // not a disjunction
                AndTerm(VariableTerm("a"), VariableTerm("b")),
                Sequent(VariableTerm("a"), VariableTerm("x")),
                Sequent(VariableTerm("b"), VariableTerm("x"))
            ), RuleFailure(e2)
        )

        assertEquals(
            DisjunctionElimination(
                // conclusions differ
                OrTerm(VariableTerm("a"), VariableTerm("b")),
                Sequent(VariableTerm("a"), VariableTerm("x")),
                Sequent(VariableTerm("b"), VariableTerm("y"))
            ), RuleFailure(e1)
        )
    }
