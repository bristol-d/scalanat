package scalanat.deduction

import scalanat.term.Term
import scalanat.term.Symbols

/**
 * A sequent p ⊢ q, obtained by assuming p, deriving q, and discharging.
 * Some rules need these as inputs.
 * The assumption can be empty, as sequents of the form "⊢ T" or similar
 * are allowed.
 */
case class Sequent(assumption: Option[Term], conclusion: Term):
    def out(using symbols: Symbols): String = assumption match {
        case None => s"${symbols.seq} ${conclusion.out}"
        case Some(a) => s"${a.out} ${symbols.seq} ${conclusion.out}"
    }
