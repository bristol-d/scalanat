package scalanat.proof

import scala.collection.mutable.ArrayBuffer

import scalanat.term.Term
import scalanat.deduction.Rule
import scalanat.deduction.Sequent
import scalanat.deduction.{RuleSuccess, RuleSuccessFree, RuleFailure}
import scalanat.{Result,Success,Failure}

/**
 * A line in a proof is one of:
 *   - introducing an assumption
 *   - applying a rule (special case: some rules have free terms)
 *   - discharging the last assumption to get a sequent
 */
sealed trait ProofLine

case class Assumption(t: Term) extends ProofLine
case class RuleApplication(rule: Rule, indexes: Seq[Int]) extends ProofLine
case class RuleApplicationFreeTerm(rule: Rule, indexes: Seq[Int], freeTerm: Term) extends ProofLine
case class Discharge() extends ProofLine

/**
 * A line in a validated proof is one of:
 *   - an assumption
 *   - a term
 *   - a sequent
 */
sealed trait ValidatedLine

case class VAssumption(t: Term) extends ValidatedLine
case class VTerm(t: Term) extends ValidatedLine
case class VSequent(s: Sequent) extends ValidatedLine

class ProofException(val msg: String, val steps: Seq[(Int, Term|Sequent)]) extends RuntimeException(msg)

case class ProofResult(val message: String, val steps: Seq[(Int, Term|Sequent)])

object Proof:
    def apply(lines: String): Result[ProofResult] =
        ProofParser(lines) match {
            case Success(items) => Proof(items)
            // A failure from the parser carries no evaluation information
            // as we've not started evaluating yet.
            case Failure(m, _) => Failure(m, None)
        }

    def apply(lines: Seq[ProofLine]): Result[ProofResult] =
        try 
            val s = apply2(lines)
            Success(s)
        catch
            case p: ProofException => Failure(p.msg, Some(ProofResult("Error", p.steps)))

    def applyDischarge(dline: Discharge, 
                       _activemap:  Map[Int, Boolean], 
                       _linemap: Map[Int, ValidatedLine], 
                       counter: Int): 
                       (Map[Int, Boolean], Map[Int, ValidatedLine]) =
        var linemap = _linemap
        var activemap = _activemap
        var scan = counter - 1
        if scan <= 0 then
            throw new ProofException("discharge: cannot use on first line of proof.", Seq())

        // we are now sure that scan actually points to something
        // the previous term must be active, and must be a term
        if !activemap(scan) then
            throw ProofException(s"line $counter: discharge: previous line is already discharged.", Seq())
        linemap(scan) match {
            case VSequent(_) => throw ProofException(s"line $counter: discharge: previous line must be a term, not a sequent.", Seq())
            case _ => ()
        }

        val c: Term = linemap(scan) match {
            case VTerm(t) => t
            // This case is a "t ⊢ t" which is sometimes needed.
            case VAssumption(t) => t 
            case _ => throw ProofException(s"Line $counter: discharge: previous line must contain a term.", Seq())
        }

        while scan > -1 do
            if scan == 0 then
                // we reached the start without seeing an assumption. Therefore, we proved a sequent with no antecedent.
                linemap = linemap + (counter -> VSequent(Sequent(None, c)))
            if !activemap(scan) then
                // skip already discharged lines
                scan = scan - 1
            else
                linemap(scan) match {
                    case VAssumption(a) => 
                        activemap = activemap + (scan -> false)
                        scan = -1 // terminate the scan
                        linemap = linemap + (counter -> VSequent(Sequent(Some(a), c)))
                    case _ =>
                        activemap = activemap + (scan -> false)
                }
        (activemap, linemap)

    def applyRule(rline: RuleApplication, 
                  _activemap:  Map[Int, Boolean], 
                  _linemap: Map[Int, ValidatedLine], 
                  counter: Int): (Map[Int, Boolean], Map[Int, ValidatedLine]) =
        var activemap = _activemap
        var linemap = _linemap
        var (rule, indexes) = rline match {
            case RuleApplication(r, i) => (r, i)
        }
        checkIndexes(indexes, activemap, counter)
        val params = mapParams(indexes, linemap)
        // the syntax here lifts a seq to varargs
        rule.apply(params:_*) match {
            case RuleSuccess(t) => 
                linemap = linemap + (counter -> VTerm(t))
            case RuleFailure(m) =>
                throw ProofException(s"Line $counter: $m", Seq())
            case RuleSuccessFree(_) =>
                throw ProofException(s"Line $counter: this rule requires a free term.", Seq())
        }
        (activemap, linemap)

    def applyRuleFree(rline: RuleApplicationFreeTerm, 
                  _activemap:  Map[Int, Boolean], 
                  _linemap: Map[Int, ValidatedLine], 
                  counter: Int): (Map[Int, Boolean], Map[Int, ValidatedLine]) =
        var activemap = _activemap
        var linemap = _linemap
        var (rule, indexes, free) = rline match {
            case RuleApplicationFreeTerm(r, i, f) => (r, i, f)
        }
        checkIndexes(indexes, activemap, counter)
        val params = mapParams(indexes, linemap)
        rule.apply(params:_*) match {
            case RuleSuccess(t) => 
                throw ProofException(s"Line $counter: this rule does not take a free term.", Seq())
            case RuleFailure(m) =>
                throw ProofException(s"Line $counter: $m", Seq())
            case RuleSuccessFree(f) =>
                linemap = linemap + (counter -> VTerm(f(free)))
        }
        (activemap, linemap)

    def mapParams(indexes: Seq[Int], linemap: Map[Int,ValidatedLine]): Seq[Term|Sequent] =
        indexes map {i => linemap(i) match {
            case VAssumption(t) => t
            case VTerm(t) => t
            case VSequent(s) => s
        }}

    def checkIndexes(indexes: Seq[Int], activemap: Map[Int, Boolean], counter: Int): Unit =
        indexes.foreach { i =>
            if !activemap.contains(i) then
                throw ProofException(s"Line $counter: No previous line $i at this point.", Seq())
            if !activemap(i) then
                throw ProofException(s"Line $counter: line $i is already discharged.", Seq())
        }

    def apply2(lines: Seq[ProofLine]): ProofResult =
        var counter = 1
        var activemap = Map[Int, Boolean]()
        var linemap = Map[Int, ValidatedLine]()

        // This is ugly, but it's for debugging purposes: we want the list of 
        // succeeded steps even if the proof as a whole fails.
        val steps = ArrayBuffer[(Int, Term|Sequent)]()

        for line <- lines do
            activemap = activemap + (counter -> true)
            try {
                line match {
                    case Assumption(t) => linemap = linemap + (counter -> VAssumption(t))
                    case d @ Discharge() =>
                        val (a, l) = applyDischarge(d, activemap, linemap, counter)
                        activemap = a
                        linemap = l
                    case r @ RuleApplication(rule, indexes) =>
                        val (a, l) = applyRule(r, activemap, linemap, counter)
                        activemap = a
                        linemap = l
                    case r @ RuleApplicationFreeTerm(rule, indexes, free) =>
                        val (a, l) = applyRuleFree(r, activemap, linemap, counter)
                        activemap = a
                        linemap = l
                }
            } catch {
                // The reason for this try-catch is that instead of passing the
                // context down to the helper methods, we keep in encapsulated
                // in this method, but backpatch it into an exception if any
                // of the methods we call throws one.
                case e : ProofException =>
                    throw ProofException(e.msg, steps.toSeq)
            }

            val step: (Int,Term|Sequent) = (counter, linemap(counter) match {
                case VAssumption(t) => t
                case VTerm(t) => t
                case VSequent(s) => s
            })
            steps.append(step)
            counter = counter + 1

        // We've succeeded. Now check what we've succeeded at.
        val noOpenAssumptions = (1 until counter).foldLeft(true) {
            (acc, index) => acc & (!activemap(index) || (linemap(index) match {
                case VAssumption(_) => false
                case _ => true
            }))
        }

        linemap(counter - 1) match {
            case VSequent(Sequent(Some(a), b)) =>
                if noOpenAssumptions then
                    ProofResult(s"Proved: ${a.out} ⊢ ${b.out}", steps.toSeq)
                else
                    ProofResult("Proof validated, but with open assumptions.", steps.toSeq)
            case VSequent(Sequent(None, b)) =>
                if noOpenAssumptions then
                    ProofResult(s"Proved: ⊢ ${b.out}", steps.toSeq)
                else
                    ProofResult("Proof validated, but with open assumptions.", steps.toSeq)
            case _ =>
                ProofResult("Proof validated, but did not derive a sequent.", steps.toSeq)
        }
