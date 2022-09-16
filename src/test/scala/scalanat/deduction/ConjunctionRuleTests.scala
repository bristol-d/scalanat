package scalanat.deduction

import scalanat.term.*

class ConjunctionRuleTests extends munit.FunSuite:

    test ("Introducing a conjunction") {
        val ta = VariableTerm("a")
        val tb = OrTerm(ValueTerm(true), VariableTerm("b"))
        assertEquals(ConjunctionIntroduction(ta, tb), RuleSuccess(
            AndTerm(ta, tb)
        ))
    }

    test ("Left eliminating a conjunction") {
        val ta = VariableTerm("a")
        val tb = VariableTerm("b")
        assertEquals(ConjunctionElimination1(AndTerm(ta, tb)), 
            RuleSuccess(ta)
        )
    }

    test ("Right eliminating a conjunction") {
        val ta = VariableTerm("a")
        val tb = VariableTerm("b")
        assertEquals(ConjunctionElimination2(AndTerm(ta, tb)), 
            RuleSuccess(tb)
        )
    }

    test ("Failing to apply conjunction elimination") {
        val ta = VariableTerm("a")
        val tb = VariableTerm("b")

        assertEquals(ConjunctionElimination1(OrTerm(ta, tb)), 
            RuleFailure("∧E1 requires one term parameter, which must be a conjunction.")
        )
    }
