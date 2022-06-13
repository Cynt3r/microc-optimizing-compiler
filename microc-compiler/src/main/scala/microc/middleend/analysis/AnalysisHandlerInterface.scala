package microc.middleend.analysis

import microc.frontend.ast.Program
import microc.middleend.cfg.ProgramCfg

/**
 * Default analysis handler, that is used in compiler to retrieve results of various analyses
 * To add support for certain analysis, simply extend this trait and override respective handler method
 * Note that implementation of a type analysis and declaration analysis is required
 */
trait AnalysisHandlerInterface {
  /** Returns result of a type analysis */
  def getTypes(program: Program): Types

  /** Returns result of a declaration analysis */
  def getDeclarations(program: Program): Declarations

  /** Returns result of a sign analysis */
  def getSigns(cfg: ProgramCfg)(implicit declarations: Declarations): Option[Signs] = None

  /** Returns result of a constant propagation analysis */
  def getConstants(cfg: ProgramCfg)(implicit declarations: Declarations): Option[Constants] = None

  /** Returns result of a live variable analysis */
  def getLiveVars(cfg: ProgramCfg)(implicit declarations: Declarations): Option[LiveVars] = None

  /** Returns result of a available expression analysis */
  def getAvailableExps(cfg: ProgramCfg)(implicit declarations: Declarations): Option[AvailableExps] = None
}
