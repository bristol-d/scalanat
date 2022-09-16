package scalanat.proof

import scalanat.term.Term
import scalanat.{Result,Success,Failure}
import scalanat.parser.{Parser,ParserProblem}
import scalanat.deduction.*

object ProofParser:
    val RULES = Map(
        "andI" -> ConjunctionIntroduction,
        "andE1" -> ConjunctionElimination1,
        "andE2" -> ConjunctionElimination2,
        "orI1" -> DisjunctionIntroduction1,
        "orI2" -> DisjunctionIntroduction2,
        "orE" -> DisjunctionElimination,
        "notI" -> NegationIntroduction,
        "notE" -> NegationElimination,
        "lem" -> LawOfExcludedMiddle,
        "falsum" -> Falsum,
        "impI" -> ImplicationIntroduction,
        "impE" -> ImplicationElimination
    )

    def apply(s: String): Result[Seq[ProofLine]] =
        val lines: Seq[Result[ProofLine]] = s.split("\n").map(_.strip)
            .filter(l => l != "").zipWithIndex.map(parseLine)
        Result.applySeq(lines)

    val RULE_LINE = "rule ([^ ]+) +([0-9, ]+)$".r
    val RULE_LINE_FREE = "rule ([^ ]+) +([0-9, ]+) +@(.*)".r

    def parseLine(line: String, index: Int): Result[ProofLine] = line match {
        case s"assume $t" => Parser(t) match {
            case ParserProblem(msg) => Failure(s"Line ${index+1}: Parser: $msg")
            case t: Term => Success(Assumption(t))
        }
        case "discharge" => Success(Discharge())
        case RULE_LINE(name, args) =>
            if RULES.contains(name) then
                val rule = RULES(name)
                val lines: Seq[Int] = try
                    args.split(",").map(_.strip).map(_.toInt)
                catch case e: NumberFormatException =>
                    return Failure(s"Line ${index+1}: Failed to parse a line number.")
                if lines.length != rule.length then
                    Failure(s"Line ${index+1}: Rule '${rule.name}' requires ${rule.length} arguments but ${lines.length} were found.")
                Success(RuleApplication(rule, lines))                
            else
                Failure(s"Line ${index+1}: No rule named '$name'.")
        case RULE_LINE_FREE(name, args, ft) =>
            if RULES.contains(name) then
                val rule = RULES(name)
                val lines: Seq[Int] = try
                    args.split(",").map(_.strip).map(_.toInt)
                catch case e: NumberFormatException =>
                    return Failure(s"Line ${index+1}: Failed to parse a line number.")
                if lines.length != rule.length then
                    Failure(s"Line ${index+1}: Rule '${rule.name}' requires ${rule.length} arguments but ${lines.length} were found.")
                Parser(ft) match {
                    case ParserProblem(e) => return Failure(s"Line ${index+1}: Failed to parse free term: $e")
                    case t: Term => Success(RuleApplicationFreeTerm(rule, lines, t))
                }
            else
                Failure(s"Line ${index+1}: No rule named '$name'.")
        
        case _ => Failure(s"Line ${index+1}: couldn't understand line.")
    }



