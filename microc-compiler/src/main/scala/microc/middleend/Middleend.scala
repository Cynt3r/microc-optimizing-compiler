package microc.middleend

import microc.Language
import microc.Language.Language
import microc.frontend.ast._
import microc.middleend.analysis.AnalysisHandlerInterface
import microc.middleend.cfg.{IntraproceduralCfgFactory, ProgramCfg}
import microc.middleend.optimization.Optimizer

/**
 * Middleend of the microC compiler that produces CFG of program's AST
 * @param handler interface for retrieving analyses
 * @param lang compiled microC language subset
 */
class Middleend(handler: AnalysisHandlerInterface, lang: Language = Language.microC) {
  /** Budget for the optimizer if optimizations are allowed */
  private val budget: Int = 500

  /**
   * Compiles AST into CFG (and optimizes the CFG if `optimize` is set to true)
   * @param program AST of the compiled program
   * @param optimize true if optimizations should be performed
   * @return CFG of the program and results of analyses of the program
   */
  def astToCfg(program: Program, optimize: Boolean = false): (ProgramCfg, AnalysesDb) = {
    if (!isValidAst(program)) {
      throw new ProgramException(s"Program contains constructs not supported by language $lang")
    }
    val factory = new IntraproceduralCfgFactory
    val cfg = factory.fromProgram(program)

    //set the budget for the optimizer
    val usedBudget = if (optimize) budget else 0

    val optimizer = new Optimizer(cfg, handler, usedBudget)
    val analyses = optimizer.run()
    (cfg, analyses)
  }

  /** Checks if Program is valid (i.e., doesn't contain any AST nodes not supported by the language) */
  private def isValidAst(program: Program): Boolean = program.tree.forall(classifyAstNode(_) <= lang)

  /** Classifies language subset in which the AstNode belongs */
  private def classifyAstNode(ast: AstNode): Language = ast match {
    case AssignStmt(lhs, _, _) => lhs match {
      case DirectWrite(_) => Language.microCVar
      case IndirectWrite(_) => Language.microC
      case DirectFieldWrite(_, _, _) => Language.microCRec
      case IndirectFieldWrite(_, _, _) => Language.microC
      case ArrayWrite(_, _, _) => Language.microCArr
    }
    case _: IfStmt => Language.microCIf
    case _: WhileStmt => Language.microCWhile
    case _: Record | _: FieldAccess => Language.microCRec
    case _: Array | _: ArrayAccess => Language.microCArr
    case _: CallFuncExpr => Language.microCFun
    case _: Null | _: Deref | _: Alloc | _: VarRef => Language.microC
    case _ => Language.microCVar
  }
}
