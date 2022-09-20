package scalanat.deduction

import scalanat.term.Term

/**
 * A sequent p ⊢ q, obtained by assuming p, deriving q, and discharging.
 * Some rules need these as inputs.
 * The assumption can be empty, as sequents of the form "⊢ T" or similar
 * are allowed.
 */
case class Sequent(assumption: Option[Term], conclusion: Term)
