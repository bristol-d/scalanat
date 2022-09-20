package scalanat.proof

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

class ProofException(val msg: String) extends RuntimeException(msg)

object Proof:
    def apply(lines: String): Result[String] =
        ProofParser(lines) match {
            case Success(items) => Proof(items)
            case Failure(s) => Failure(s)
        }

    def apply(lines: Seq[ProofLine]): Result[String] =
        try 
            val s = apply2(lines)
            Success(s)
        catch
            case p: ProofException => Failure(p.msg)

    def applyDischarge(dline: Discharge, _activemap:  Map[Int, Boolean], _linemap: Map[Int, ValidatedLine], counter: Int): (Map[Int, Boolean], Map[Int, ValidatedLine]) =
        var linemap = _linemap
        var activemap = _activemap
        var scan = counter - 1
        if scan <= 0 then
            throw new ProofException("discharge: cannot use on first line of proof.")

        // we are now sure that scan actually points to something
        // the previous term must be active, and must be a term
        if !activemap(scan) then
            throw ProofException(s"line $counter: discharge: previous line is already discharged.")
        linemap(scan) match {
            case VSequent(_) => throw ProofException(s"line $counter: discharge: previous line must be a term, not a sequent.")
            case _ => ()
        }

        val c: Term = linemap(scan) match {
            case VTerm(t) => t
            // This case is a "t ⊢ t" which is sometimes needed.
            case VAssumption(t) => t 
            case _ => throw ProofException(s"Line $counter: discharge: previous line must contain a term.")
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
                throw ProofException(s"Line $counter: $m")
            case RuleSuccessFree(_) =>
                throw ProofException(s"Line $counter: this rule requires a free term.")
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
                throw ProofException(s"Line $counter: this rule does not take a free term.")
            case RuleFailure(m) =>
                throw ProofException(s"Line $counter: $m")
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
                throw ProofException(s"Line $counter: No previous line $i at this point.")
            if !activemap(i) then
                throw ProofException(s"Line $counter: line $i is already discharged.")
        }

    def apply2(lines: Seq[ProofLine]): String =
        var counter = 1
        var activemap = Map[Int, Boolean]()
        var linemap = Map[Int, ValidatedLine]()

        for line <- lines do
            activemap = activemap + (counter -> true)
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
                    s"Proved: ${a.out} ⊢ ${b.out}"
                else
                    "Proof validated, but with open assumptions."
            case VSequent(Sequent(None, b)) =>
                if noOpenAssumptions then
                    s"Proved: ⊢ ${b.out}"
                else
                    "Proof validated, but with open assumptions."
            case _ =>
                "Proof validated, but did not derive a sequent."
        }
