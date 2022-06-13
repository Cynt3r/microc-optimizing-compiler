package microc.middleend.optimization

import microc.frontend.ast.{Expr, Number}
import microc.middleend.cfg.CfgNode

/** Basic dead code elimination that requires no prior analysis */
class BasicDeadCodeElimination() extends DeadCodeElimination {
  val cost: Int = 1

  //always runnable, since no analysis is required
  def isRunnable: Boolean = true

  protected def isTrue(expr: Expr, nodes: Set[CfgNode]): Boolean = expr match {
    case Number(value, _) if value != 0 => true
    case _ => false
  }

  protected def isFalse(expr: Expr, nodes: Set[CfgNode]): Boolean = expr match {
    case Number(0, _) => true
    case _ => false
  }
}
