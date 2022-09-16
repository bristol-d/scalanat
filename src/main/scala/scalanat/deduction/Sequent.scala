package scalanat.deduction

import scalanat.term.Term

/**
 * A sequent p ⊢ q, obtained by assuming p, deriving q, and discharging.
 * Some rules need these as inputs.
 */
case class Sequent(assumption: Term, conclusion: Term)
