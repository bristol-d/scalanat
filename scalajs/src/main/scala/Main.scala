import scalanat.proof.Proof
import scalanat.{Result,Success,Failure}
import scalanat.term.Term
import scalanat.deduction.Sequent

def remap(t: Term|Sequent): Either[Term, Sequent] = t match {
    case s: Sequent => Right(s)
    case t: Term => Left(t)
}

@main def main: Unit = 
  given scalanat.term.Symbols = scalanat.term.DefaultSymbols
  val p = Proof("""assume not a or b
                  |    assume a
                  |        assume not a
                  |            rule notE 2, 3
                  |            rule falsum 4 @ b
                  |        discharge
                  |        assume b
                  |        discharge
                  |        rule orE 1, 6, 8
                  |    discharge
                  |    rule impI 10
                  |discharge""".stripMargin)

  p match {
    case Success(pp) =>
      println(pp.message)
      pp.steps.foreach { (index, t) =>
        val data = remap(t) match {
          case Left(s) => s.out
          case Right(s) => s.out
        }
        println(f"$index%02d: ${data}")  
      }
    case f @ Failure(_, _) => println(f)
  }
