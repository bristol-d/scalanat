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

    val RULE_LINE = "rule ([^ ]+) +([0-9, ]*)$".r
    val RULE_LINE_FREE = "rule ([^ ]+) +([0-9, ]*) *@(.*)".r

    def stripIndentComment(line: String): String =
        val line2 = line.stripLeading()
        val comment = line.indexOf("#")
        if comment > 0 then
            line2.substring(0, comment).stripTrailing()
        else
            line2.stripTrailing()

    // Used when converting argument lists to integers.
    // Since an empty list will be returned as Seq("") instead of Seq(),
    // we need to handle this case specially to not get an exception.
    def maybeToInt(s: String): Seq[Int] =
        s match {
            case "" => Seq()
            case _ => Seq(s.toInt)
        }

    // method that abstracts common code for RULE and RULE_FREE
    // into a continuation
    def parseRule(name: String, args: String, index: Int)
                 (cont: (Rule, Seq[Int]) => Result[ProofLine]): 
                  Result[ProofLine] =
        if RULES.contains(name) then
            val rule = RULES(name)
            val lines: Seq[Int] = try
                args.split(",").map(_.strip).flatMap(maybeToInt(_))
            catch case e: NumberFormatException =>
                return Failure(s"Line ${index+1}: Failed to parse a line number.")
            if lines.length != rule.length then
                Failure(s"Line ${index+1}: Rule '${rule.name}' requires ${rule.length} arguments but ${lines.length} were found.")
            cont(rule, lines)               
        else
            Failure(s"Line ${index+1}: No rule named '$name'.")

    def parseLine(line: String, index: Int): Result[ProofLine] = 
        stripIndentComment(line) match {
            case s"assume $t" => Parser(t) match {
                case ParserProblem(msg) => Failure(s"Line ${index+1}: Parser: $msg")
                case t: Term => Success(Assumption(t))
            }
            case "discharge" => Success(Discharge())
            case r @ RULE_LINE(name, args) => parseRule(name, args, index) {
                (rule, lines) => Success(RuleApplication(rule, lines))
            }
            case RULE_LINE_FREE(name, args, ft) => parseRule(name, args, index) {
                (rule, lines) => Parser(ft) match {
                    case ParserProblem(e) => Failure(s"Line ${index+1}: Failed to parse free term: $e")
                    case t: Term => Success(RuleApplicationFreeTerm(rule, lines, t))
                }
            }   
            case x => Failure(s"Line ${index+1}: couldn't understand line '$x'. It must start with 'assume', 'rule' or 'discharge'.")
        }



