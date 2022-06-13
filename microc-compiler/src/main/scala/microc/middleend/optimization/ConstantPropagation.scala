package microc.middleend.optimization
import microc.frontend.ast.{ArrayWrite, AssignStmt, AstNode, Decl, Identifier, IdentifierDecl, IndirectFieldWrite, IndirectWrite, Number, VarRef}
import microc.middleend.AnalysesDb
import microc.middleend.cfg.{CfgStmtNode, ProgramCfg}
import microc.middleend.lattice.{Constant, FlatTop, FlatVal}

/** Optimization that propagates number constants based on constant propagation analysis */
class ConstantPropagation(analyses: AnalysesDb) extends Optimization {
  val cost: Int = 1

  //constant propagation analysis required
  def isRunnable: Boolean = analyses.constants.isDefined

  def run(cfg: ProgramCfg): List[OptimizationAction] = {
    collectCfgStmts(cfg).flatMap(stmt => collectFromAst(stmt.ast, stmt))
  }

  private def collectFromAst(ast: AstNode, stmt: CfgStmtNode): List[OptimizationAction] = ast match {
    case AssignStmt(lhs, rhs, _) =>
      val lhsActions = lhs match {
        case IndirectWrite(expr) => collectFromAst(expr, stmt)
        case IndirectFieldWrite(expr, _, _) => collectFromAst(expr, stmt)
        case ArrayWrite(expr1, expr2, _) => collectFromAst(expr1, stmt) ::: collectFromAst(expr2, stmt)
        //lhs of other assignables cannot be replaced
        case _ => List()
      }
      lhsActions ::: collectFromAst(rhs, stmt)

    //variable references cannot be replaced
    case _: VarRef => List()

    case id: Identifier if analyses.declarations(id).isInstanceOf[IdentifierDecl] =>
      val decl = analyses.declarations(id)
      getConstant(decl, stmt) match {
        case FlatVal(value) => List(ReplaceNode(id, Number(value, id.loc), stmt))
        case _ => List()
      }

    case _ =>
      ast.children
        .toList
        .flatMap(node => collectFromAst(node, stmt))
  }

  /** Returns constant of a declaration based on the predecessors of the node */
  private def getConstant(decl: Decl, stmt: CfgStmtNode): Constant = {
    stmt.pred
      .map(node => analyses.constants.get(node).getOrElse(decl, FlatTop)) //map predecessor to their constant value of a decl
      .reduceOption((l, r) => if (l != r) FlatTop else l)
      .getOrElse(FlatTop)
  }
}
