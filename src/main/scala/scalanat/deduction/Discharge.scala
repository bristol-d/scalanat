package scalanat.deduction

import scalanat.term.Term

/**
 * A discharged assumption. Some rules need these as inputs.
 */
case class Discharge(assumption: Term, conclusion: Term)
