package scalanat.proof

import scalanat.Success

class ProofTests extends munit.FunSuite:

    test("commutativity of and") {
        val source = """assume a and b
                       |    rule andE1 1
                       |    rule andE2 1
                       |    rule andI 3, 2
                       |discharge""".stripMargin
        val result = Proof(source)
        assertEquals(result, Success("Proved: a ∧ b ⊢ b ∧ a"))

    }

    test("associativity of and") {
        val source = """assume (a and b) and c
                       |    rule andE1 1         # 2: a and b
                       |    rule andE1 2         # 3: a
                       |    rule andE2 2         # 4: b
                       |    rule andE2 1         # 5: c
                       |    rule andI 4, 5       # 6: (b and c)
                       |    rule andI 3, 6       # 7: a and (b and c)
                       |discharge""".stripMargin
        val result = Proof(source)
        assertEquals(result, Success("Proved: (a ∧ b) ∧ c ⊢ a ∧ (b ∧ c)"))
    }

    test("def of imp, forwards") {
        val source = """assume a imp b
                       |    assume a                  #  2
                       |        rule impE 1, 2        #  3: b
                       |        rule orI2 3 @ not a   #  4: not a or b
                       |    discharge                 #  5: a ⊢ not a or b
                       |    assume not a              #  6
                       |        rule orI1 6 @ b       #  7: not a or b
                       |    discharge                 #  8: not a ⊢ not a or b
                       |    rule lem @ a              #  9: a or not a
                       |    rule orE 9, 5, 8          # 10: not a or b
                       |discharge""".stripMargin
        val result = Proof(source)
        assertEquals(result, Success("Proved: a ⇒ b ⊢ ¬a ∨ b"))
    }

    test ("def of imp, backwards") {
        val source = """assume not a or b
                       |    assume a
                       |        assume not a
                       |            rule notE 2, 3    #  4: F
                       |            rule falsum 4 @ b #  5: b
                       |        discharge             #  6: not a |- b
                       |        assume b
                       |        discharge             #  8: b |- b
                       |        rule orE 1, 6, 8      #  9: b
                       |    discharge                 # 10: a |- b
                       |    rule impI 10              # 11: a imp b
                       |discharge""".stripMargin
        val result = Proof(source)
        assertEquals(result, Success("Proved: ¬a ∨ b ⊢ a ⇒ b"))
    }
