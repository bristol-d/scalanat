import scalanat.proof.Proof
import scalanat.{Result,Success,Failure}
import scalanat.term.Term
import scalanat.deduction.Sequent

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("runProof")
def runProof(source: String): JSResult =
  given scalanat.term.Symbols = scalanat.term.DefaultSymbols
  val proof = Proof(source)
  proof match {
    case Success(r) => JSResult(
      success = true,
      message = r.message,
      trace = r.steps.map {(i, v) =>
        (i, str(v))  
      }.toArray
    )
    case Failure(msg, trace) => JSResult(
      success = false,
      message = msg,
      trace = trace match {
        case Some(tt) => tt.steps.map{(i, v) => (i, str(v))}.toArray
        case None => Seq((0, "No execution trace.")).toArray
      }
    )
  }

def remap(t: Term|Sequent): Either[Term, Sequent] = t match {
    case s: Sequent => Right(s)
    case t: Term => Left(t)
}

def str(v: Term|Sequent): String = 
  given scalanat.term.Symbols = scalanat.term.DefaultSymbols
  remap(v) match {
    case Left(x) => x.out
    case Right(x) => x.out
  }

@main def main: Unit = 
  /*
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
  */
  ()
