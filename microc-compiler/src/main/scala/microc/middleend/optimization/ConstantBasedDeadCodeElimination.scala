package microc.middleend.optimization

import microc.frontend.ast.{BinaryOp, Expr, Identifier, Number}
import microc.middleend.AnalysesDb
import microc.middleend.cfg.CfgNode
import microc.middleend.lattice.{Constant, FlatTop, FlatVal}

/** Advanced dead code elimination that utilizes the result of constant propagation analysis */
class ConstantBasedDeadCodeElimination(analyses: AnalysesDb) extends DeadCodeElimination {
  val cost: Int = 1

  //constant propagation analysis required
  def isRunnable: Boolean = analyses.constants.isDefined

  protected def isTrue(expr: Expr, nodes: Set[CfgNode]): Boolean = expr match {
    //special case where id can evaluate to multiple different constants, which all evaluate to true
    case id: Identifier if isIdTrue(id, nodes) => true

    case _ =>
      getExprConstant(expr, nodes) match {
        case FlatVal(v) if v != 0 => true
        case _ => false
      }
  }

  protected def isFalse(expr: Expr, nodes: Set[CfgNode]): Boolean = {
    getExprConstant(expr, nodes) match {
      case FlatVal(0) => true
      case _ => false
    }
  }

  private def getExprConstant(expr: Expr, nodes: Set[CfgNode]): Constant = expr match {
    case Number(value, _) => FlatVal(value)
    case BinaryOp(op, left, right, _) =>
      (getExprConstant(left, nodes), getExprConstant(right, nodes)) match {
        case (FlatVal(l), FlatVal(r)) => FlatVal(foldBinOp(op, l, r))
        case _ => FlatTop
      }

    case id: Identifier =>
      idConstants(id, nodes)
        .reduceOption((l, r) => if (l != r) FlatTop else l)
        .getOrElse(FlatTop)

    case _ => FlatTop
  }

  /** Returns list of all possible constants the id can be */
  private def idConstants(id: Identifier, nodes: Set[CfgNode]): List[Constant] = {
    val decl = analyses.declarations(id)
    nodes.map(analyses.constants.get(_)(decl)).toList
  }

  /** Returns true if the id evaluates to true based on the given nodes and their constants */
  private def isIdTrue(id: Identifier, nodes: Set[CfgNode]): Boolean = {
    idConstants(id, nodes).forall {
      case FlatVal(v) if v != 0 => true
      case _ => false
    }
  }
}
