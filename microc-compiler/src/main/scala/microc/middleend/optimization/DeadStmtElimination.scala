package microc.middleend.optimization
import microc.frontend.ast._
import microc.middleend.AnalysesDb
import microc.middleend.cfg.{CfgStmtNode, ProgramCfg}

import scala.annotation.tailrec

/** Optimization that eliminates dead assignments that have no side effect */
class DeadStmtElimination(analyses: AnalysesDb) extends Optimization {
  val cost: Int = 1

  //live variable analysis required
  def isRunnable: Boolean = analyses.liveVars.isDefined

  def run(cfg: ProgramCfg): List[OptimizationAction] = {
    collectCfgStmts(cfg).flatMap {
      case node @ CfgStmtNode(assign: AssignStmt) => collectFromStmt(assign, node)
      case _ => List()
    }
  }

  private def collectFromStmt(assign: AssignStmt, stmt: CfgStmtNode): List[OptimizationAction] = {
    //if lhs is not live in any successor nodes and rhs has no side-effects, assign can be safely deleted
    assignableToId(assign.left) match {
      case Some(decl) if isDead(decl, stmt) && hasSideEffect(assign.right) =>
        //replace the assignment with the rhs of the assignment (which contains the side-effect)
        List(ReplaceNode(stmt.ast, assign.right, stmt))

      case Some(decl) if isDead(decl, stmt) =>
        //delete whole node
        List(DeleteNode(stmt))

      case _ =>
        List()
    }
  }

  /** Extracts declaration that is being assigned from Assignable */
  private def assignableToId(a: Expr): Option[Decl] = a match {
    case DirectWrite(id) => exprToId(id)
    case IndirectWrite(expr) => exprToId(expr)
    case DirectFieldWrite(id, _, _) => exprToId(id)
    case IndirectFieldWrite(expr, _, _) => exprToId(expr)
    case ArrayWrite(expr, _, _) => exprToId(expr)
  }

  /** Extracts identifier that is being assigned from Expr */
  @tailrec
  private def exprToId(expr: Expr): Option[Decl] = expr match {
    case id: Identifier => Some(analyses.declarations(id))
    case Deref(e, _) => exprToId(e)
    case VarRef(id, _) => exprToId(id)
    case FieldAccess(record, _, _) => exprToId(record)
    case _ => None
  }

  /** Returns true if declaration is dead in all successor CFG nodes */
  private def isDead(decl: Decl, stmt: CfgStmtNode): Boolean = {
    stmt.succ.forall(node => !analyses.liveVars.get(node).contains(decl))
  }
}
