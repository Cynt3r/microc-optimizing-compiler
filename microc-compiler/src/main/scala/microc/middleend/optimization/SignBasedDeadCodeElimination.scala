package microc.middleend.optimization

import microc.frontend.ast.{BinaryOp, BinaryOperator, Divide, Equal, Expr, GreatThan, Identifier, Minus, Number, Plus, Times}
import microc.middleend.AnalysesDb
import microc.middleend.cfg.CfgNode
import microc.middleend.lattice.SignLattice.{Neg, Pos, Zer}
import microc.middleend.lattice.{FlatTop, FlatVal, Sign}

/** Advanced dead code elimination that utilizes the result of sign analysis */
class SignBasedDeadCodeElimination(analyses: AnalysesDb) extends DeadCodeElimination {
  val cost: Int = 1

  //constant propagation analysis required
  def isRunnable: Boolean = analyses.signs.isDefined

  protected def isTrue(expr: Expr, nodes: Set[CfgNode]): Boolean = expr match {
    //special case where id can evaluate to multiple different signs, which all evaluate to true
    case id: Identifier if isIdTrue(id, nodes) => true

    case _ =>
      getExprSign(expr, nodes) match {
        case FlatVal(Pos) => true
        case FlatVal(Neg) => true
        case _ => false
      }
  }

  protected def isFalse(expr: Expr, nodes: Set[CfgNode]): Boolean = {
    getExprSign(expr, nodes) match {
      case FlatVal(Zer) => true
      case _ => false
    }
  }

  /** Returns sign of an expression */
  private def getExprSign(expr: Expr, nodes: Set[CfgNode]): Sign = expr match {
    case Number(0, _) => FlatVal(Zer)
    case Number(n, _) if n > 0 => FlatVal(Pos)
    case Number(_, _) => FlatVal(Neg)

    case BinaryOp(op, left, right, _) =>
      val lSign = getExprSign(left, nodes)
      val rSign = getExprSign(right, nodes)
      foldSign(op, lSign, rSign)

    case id: Identifier =>
      idSigns(id, nodes)
        .reduceOption((l, r) => if (l != r) FlatTop else l)
        .getOrElse(FlatTop)

    case _ => FlatTop
  }

  /** Folds binary operator into a sign */
  private def foldSign(op: BinaryOperator, l: Sign, r: Sign): Sign = (op, l, r) match {
    case (Plus, FlatVal(Neg), FlatVal(Neg)) => FlatVal(Neg)
    case (Plus, FlatVal(Neg), FlatVal(Zer)) => FlatVal(Neg)
    case (Plus, FlatVal(Zer), FlatVal(Neg)) => FlatVal(Neg)
    case (Plus, FlatVal(Zer), FlatVal(Zer)) => FlatVal(Zer)
    case (Plus, FlatVal(Zer), FlatVal(Pos)) => FlatVal(Pos)
    case (Plus, FlatVal(Pos), FlatVal(Zer)) => FlatVal(Pos)
    case (Plus, FlatVal(Pos), FlatVal(Pos)) => FlatVal(Pos)

    case (Minus, FlatVal(Neg), FlatVal(Zer)) => FlatVal(Neg)
    case (Minus, FlatVal(Neg), FlatVal(Pos)) => FlatVal(Neg)
    case (Minus, FlatVal(Zer), FlatVal(Neg)) => FlatVal(Pos)
    case (Minus, FlatVal(Zer), FlatVal(Zer)) => FlatVal(Zer)
    case (Minus, FlatVal(Zer), FlatVal(Pos)) => FlatVal(Neg)
    case (Minus, FlatVal(Pos), FlatVal(Neg)) => FlatVal(Pos)
    case (Minus, FlatVal(Pos), FlatVal(Zer)) => FlatVal(Pos)

    case (Times, FlatVal(Neg), FlatVal(Neg)) => FlatVal(Pos)
    case (Times, FlatVal(Neg), FlatVal(Pos)) => FlatVal(Pos)
    case (Times, FlatVal(Zer), FlatVal(_)) => FlatVal(Zer)
    case (Times, FlatVal(_), FlatVal(Zer)) => FlatVal(Zer)
    case (Times, FlatVal(Pos), FlatVal(Neg)) => FlatVal(Neg)
    case (Times, FlatVal(Pos), FlatVal(Pos)) => FlatVal(Pos)

    case (Divide, FlatVal(Zer), FlatVal(Neg)) => FlatVal(Zer)
    case (Divide, FlatVal(Zer), FlatVal(Pos)) => FlatVal(Zer)


    case (Equal, FlatVal(Neg), FlatVal(Zer)) => FlatVal(Zer)
    case (Equal, FlatVal(Neg), FlatVal(Pos)) => FlatVal(Zer)
    case (Equal, FlatVal(Zer), FlatVal(Neg)) => FlatVal(Zer)
    case (Equal, FlatVal(Zer), FlatVal(Zer)) => FlatVal(Pos)
    case (Equal, FlatVal(Zer), FlatVal(Pos)) => FlatVal(Zer)
    case (Equal, FlatVal(Pos), FlatVal(Neg)) => FlatVal(Zer)
    case (Equal, FlatVal(Pos), FlatVal(Zer)) => FlatVal(Zer)

    case (GreatThan, FlatVal(Neg), FlatVal(Zer)) => FlatVal(Zer)
    case (GreatThan, FlatVal(Neg), FlatVal(Pos)) => FlatVal(Zer)
    case (GreatThan, FlatVal(Zer), FlatVal(Neg)) => FlatVal(Pos)
    case (GreatThan, FlatVal(Zer), FlatVal(Zer)) => FlatVal(Zer)
    case (GreatThan, FlatVal(Zer), FlatVal(Pos)) => FlatVal(Zer)
    case (GreatThan, FlatVal(Pos), FlatVal(Neg)) => FlatVal(Pos)
    case (GreatThan, FlatVal(Pos), FlatVal(Zer)) => FlatVal(Pos)

    case _ => FlatTop
  }

  /** Returns list of all possible signs the id can be */
  private def idSigns(id: Identifier, nodes: Set[CfgNode]): List[Sign] = {
    val decl = analyses.declarations(id)
    nodes.map(analyses.signs.get(_)(decl)).toList
  }

  /** Returns true if the id evaluates to true based on the given nodes and their signs */
  private def isIdTrue(id: Identifier, nodes: Set[CfgNode]): Boolean = {
    idSigns(id, nodes).forall {
      case FlatVal(Pos) => true
      case FlatVal(Neg) => true
      case _ => false
    }
  }
}
