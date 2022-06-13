package microc.middleend.optimization

import microc.frontend.ast.Expr
import microc.middleend.cfg.{CfgNode, CfgNodeContexts, CfgStmtNode, DoWhileContext, IfContext, ProgramCfg, WhileContext}

/** Abstract representation of an dead code elimination */
trait DeadCodeElimination extends Optimization {
  def run(cfg: ProgramCfg): List[OptimizationAction] = {
    collectCfgStmts(cfg)
      .view //lazily
      .map(stmt => collectFromStmt(stmt, cfg.contexts)) //collect actions from the stmt
      .find(_.nonEmpty) //find the first stmt that returns at least one action
      .getOrElse(List())
  }

  private def collectFromStmt(stmt: CfgStmtNode, contexts: CfgNodeContexts): List[OptimizationAction] = {
    (contexts(stmt), stmt.ast) match {
      //while (non-zero integer on first evaluation of the guard) {x} ... -> do {x} while(guard) ...
      //first evaluation = depends on the state of predecessor nodes of the whole while-statement
      case (WhileContext(Some(bodyBranch)), guard: Expr) if isTrue(guard, (stmt.pred -- bodyBranch.exit).toSet) =>
        whileToDoWhile(stmt, bodyBranch)

      //while (0 on first evaluation of the guard) {x} ... -> ...
      case (WhileContext(Some(bodyBranch)), guard: Expr) if isFalse(guard, (stmt.pred -- bodyBranch.exit).toSet) =>
        //delete guard
        val deleteNode = DeleteNode(stmt)
        //disconnect whole body branch from cfg
        val disconnectNodes = disconnectBranch(Some(bodyBranch))
        deleteNode :: disconnectNodes

      //if (0) {x} else {y} -> {y}
      case (IfContext(thenBranch, _), guard: Expr) if isFalse(guard, stmt.pred.toSet) =>
        //delete guard
        val deleteNode = DeleteNode(stmt)
        //disconnect whole then branch from cfg
        val disconnectNodes = disconnectBranch(thenBranch)
        deleteNode :: disconnectNodes

      //if (non-zero integer) {x} else {y} -> {x}
      case (IfContext(_, elseBranch), guard: Expr) if isTrue(guard, stmt.pred.toSet) =>
        //delete guard
        val deleteNode = DeleteNode(stmt)
        //disconnect whole else branch from cfg
        val disconnectNodes = disconnectBranch(elseBranch)
        deleteNode :: disconnectNodes

      //do {x} while (0) ... -> x ...
      case (DoWhileContext(Some(bodyBranch), guard @ CfgStmtNode(guardExpr: Expr)), _) if isFalse(guardExpr, guard.pred.toSet) =>
        //disconnect guard
        val disconnectNode = DisconnectNode(guard, keepContexts = false)
        //connect exit nodes of the body branch to successors of the guard
        val connectNodes = bodyBranch.exit
          .flatMap(exitNode => {
            guard.succ
              .filterNot(_ == bodyBranch.entry) //successors of guard that are not the entry of the body
              .map(n => ConnectNodes(exitNode.asInstanceOf[CfgStmtNode], n.asInstanceOf[CfgStmtNode]))
          })
          .toList
        connectNodes ::: List(disconnectNode) //order is important to correctly preserve contexts

      //do {} while (0) ... -> ...
      case (DoWhileContext(None, guard @ CfgStmtNode(guardExpr: Expr)), _) if isFalse(guardExpr, guard.pred.toSet) =>
        List(DeleteNode(guard))

      case _ =>
        List()
    }
  }

  /** Returns true if the expression evaluates to true based on the state of given nodes */
  protected def isTrue(expr: Expr, nodes: Set[CfgNode]): Boolean

  /** Returns true if the expression evaluates to false based on the state of given nodes */
  protected def isFalse(expr: Expr, nodes: Set[CfgNode]): Boolean
}
