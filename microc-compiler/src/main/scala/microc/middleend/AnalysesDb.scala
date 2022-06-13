package microc.middleend

import microc.middleend.analysis._

/**
 * Wrapper containing all analyses of a program
 * @param types result of a type analysis
 * @param declarations result of a declaration analysis
 * @param signs result of a sign analysis or None
 * @param constants result of a constant propagation analysis or None
 * @param liveVars result of a live variable analysis or None
 * @param availableExps result of an available expression analysis or None
 */
case class AnalysesDb(
  types: Types,
  declarations: Declarations,
  signs: Option[Signs],
  constants: Option[Constants],
  liveVars: Option[LiveVars],
  availableExps: Option[AvailableExps],
)
