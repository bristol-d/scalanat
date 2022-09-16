package scalanat.proof

import scalanat.{Result,Success,Failure}
import scalanat.term.*
import scalanat.deduction.ConjunctionIntroduction
import scalanat.deduction.DisjunctionIntroduction1
import scalanat.deduction.ImplicationElimination

class ProofParserTests extends munit.FunSuite:
    
    test("parsing an assumption line") {
        assertEquals(
            ProofParser.parseLine("assume p and q", 5),
            Success(Assumption(
                AndTerm(
                    VariableTerm("p"),
                    VariableTerm("q")
                )
            ))
        )
    }

    test("parsing a discharge line") {
        assertEquals(
            ProofParser.parseLine("discharge", 5),
            Success(Discharge())
        )
    }

    test("parsing a rule line") {
        assertEquals(
            ProofParser.parseLine("rule andI 2, 3", 5),
            Success(RuleApplication(ConjunctionIntroduction, Seq(2, 3)))
        )
    }

    test("parsing a rule line with a free term") {
        assertEquals(
            ProofParser.parseLine("rule orI1 2 @ not q", 5),
            Success(RuleApplicationFreeTerm(DisjunctionIntroduction1, Seq(2), NotTerm(VariableTerm("q"))))
        )
    }

    test("proof parser") {
        val source = """assume p imp q
                       |assume p
                       |rule impE 1,2
        """.stripMargin
        assertEquals(
            ProofParser(source),
            Success(Seq(
                Assumption(ImpTerm(VariableTerm("p"), VariableTerm("q"))),
                Assumption(VariableTerm("p")),
                RuleApplication(ImplicationElimination, Seq(1, 2))
            ))
        )
    }

