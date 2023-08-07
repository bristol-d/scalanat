package scalanat.proof

import scalanat.{Result,Success,Failure}
import scalanat.term.Term
import scalanat.deduction.Sequent
import scalanat.term.{Symbols, AsciiSymbols}

class ProofTests extends munit.FunSuite:

    given Symbols = AsciiSymbols

    // This whole method and its use should not exist, but the typechecker
    // yells at me if I don't do this and it's too late in the evening
    // to find out why.
    def remap(t: Term|Sequent): Either[Term, Sequent] = t match {
        case s: Sequent => Right(s)
        case t: Term => Left(t)
    }

    def assertSuccess(result: Result[ProofResult], msg: String) = 
        result match {
            case Success(ProofResult(m, _)) => assertEquals(m, msg)
            case Failure(m, None) => assert(false, "FAILED:\n" + m)
            case Failure(m, Some(ProofResult(mm, steps))) => 
                assert(false, "\n" + m + "\nContext:\n" +
                    steps.map((index, t) => {
                        remap(t) match {
                            case Left(l) => f"$index%02d: ${l.out}"
                            case Right(r) => f"$index%02d: ${r.out}"
                        }
                }).mkString("\n"))
        }

    test("commutativity of and") {
        val source = """assume a and b
                       |    rule andE1 1
                       |    rule andE2 1
                       |    rule andI 3, 2
                       |discharge""".stripMargin
        val result = Proof(source)
        assertSuccess(result, "Proved: a & b #- b & a")

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
        assertSuccess(result, "Proved: (a & b) & c #- a & (b & c)")
    }

    test("def of imp, forwards") {
        val source = """assume a imp b
                       |    assume a                  #  2
                       |        rule impE 1, 2        #  3: b
                       |        rule orI2 3 @ not a   #  4: not a or b
                       |    discharge                 #  5: a #- not a or b
                       |    assume not a              #  6
                       |        rule orI1 6 @ b       #  7: not a or b
                       |    discharge                 #  8: not a |- not a or b
                       |    rule lem @ a              #  9: a or not a
                       |    rule orE 9, 5, 8          # 10: not a or b
                       |discharge""".stripMargin
        val result = Proof(source)
        assertSuccess(result, "Proved: a -> b #- ~a | b")
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
        assertSuccess(result, "Proved: ~a | b #- a -> b")
    }

    test("commutativity of or") {
        val source = """assume a or b
                       |    assume a
                       |        rule orI2 2 @ b  # B or A
                       |    discharge            # A |- B or A
                       |    assume b
                       |        rule orI1 5 @ a  # B or A
                       |    discharge            # B |- B or A
                       |    rule orE 1, 4, 7
                       |discharge
                       |""".stripMargin
        val result = Proof(source)
        assertSuccess(result, "Proved: a | b #- b | a")
    }

    test("assocativity of or") {
        val source = """assume a or (b or c)
                       |    assume a
                       |        rule orI1 2 @ b
                       |        rule orI1 3 @ c
                       |    discharge                # a |- (a | b) | c
                       |    assume b
                       |        rule orI2 6 @ a
                       |        rule orI1 7 @ c
                       |    discharge                # b |- (a | b) | c
                       |    assume c
                       |        rule orI2 10 @ a or b
                       |    discharge                # c |- (a | b) | c
                       |    assume b or c
                       |        rule orE 13, 9, 12
                       |    discharge                # b | c |- (a | b) | c
                       |    rule orE 1, 5, 15
                       |discharge
                       |""".stripMargin
        val result = Proof(source)
        assertSuccess(result, "Proved: a | (b | c) #- (a | b) | c")
    }

    test("referring to future line produces correct error") {
        val source = """assume a and b
                       |    rule andE1 3
                       |discharge""".stripMargin
        val result = Proof(source)
        result match {
            case Success(_) => assert(false, "should not succeed")
            case Failure(m, _) => assertEquals(m, "Line 2: No previous line 3 at this point.")
        }
    }

    test("referring to current line produces correct error") {
        val source = """assume a and b
                       |    rule andE1 2
                       |discharge""".stripMargin
        val result = Proof(source)
        result match {
            case Success(_) => assert(false, "should not succeed")
            case Failure(m, _) => assertEquals(m, "Line 2: Line cannot refer to itself.")
        }
    }

