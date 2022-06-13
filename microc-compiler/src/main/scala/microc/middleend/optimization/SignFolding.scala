package microc.middleend.optimization

import microc.frontend.ast.{AstNode, BinaryOp, BinaryOperator, Decl, Equal, GreatThan, Identifier, Number}
import microc.middleend.AnalysesDb
import microc.middleend.cfg.{CfgStmtNode, ProgramCfg}
import microc.middleend.lattice.SignLattice.{Neg, Pos, Zer}
import microc.middleend.lattice.{FlatTop, FlatVal, Sign}

/** Optimization that performs sign folding based on sign analysis */
class SignFolding(analyses: AnalysesDb) extends Optimization {
  val cost: Int = 1

  //sign analysis required
  def isRunnable: Boolean = analyses.signs.isDefined

  def run(cfg: ProgramCfg): List[OptimizationAction] = {
    collectCfgStmts(cfg)
      .flatMap(stmt => {
        //collect optimization from each sub-ast-node of a stmt
        stmt.ast.tree.flatMap(ast => collectFromAst(ast, stmt))
      })
  }

  private def collectFromAst(ast: AstNode, stmt: CfgStmtNode): List[OptimizationAction] = ast match {
    case binOp @ BinaryOp(op, lhs @ Identifier(_, _), rhs @ Identifier(_, _), _) if op == Equal || op == GreatThan =>
      val lDecl = analyses.declarations(lhs)
      val rDecl = analyses.declarations(rhs)
      val lSign = getSign(lDecl, stmt)
      val rSign = getSign(rDecl, stmt)
      extractActions(binOp, lSign, rSign, stmt)

    case binOp @ BinaryOp(op, Number(value, _), id @ Identifier(_, _), _) if op == Equal || op == GreatThan =>
      val decl = analyses.declarations(id)
      val lSign = classifyNum(value)
      val rSign = getSign(decl, stmt)
      extractActions(binOp, lSign, rSign, stmt)

    case binOp @ BinaryOp(op, id @ Identifier(_, _), Number(value, _), _) if op == Equal || op == GreatThan =>
      val decl = analyses.declarations(id)
      val lSign = getSign(decl, stmt)
      val rSign = classifyNum(value)
      extractActions(binOp, lSign, rSign, stmt)

    case id: Identifier if getSign(analyses.declarations(id), stmt) == classifyNum(0) =>
      List(ReplaceNode(id, Number(0, id.loc), stmt))

    case _ => List()
  }

  /** Returns sign of a number */
  private def classifyNum(num: Int): Sign = num match {
    case 0 => Zer
    case _ if num < 0 => Neg
    case _ => Pos
  }

  /** Extracts optimization actions from a binary operation */
  private def extractActions(binOp: BinaryOp, lhs: Sign, rhs: Sign, stmt: CfgStmtNode): List[OptimizationAction] = {
    foldBinOp(binOp.operator, lhs, rhs) match {
      case Some(n) => List(ReplaceNode(binOp, Number(n, binOp.loc), stmt))
      case None => List()
    }
  }

  /** Folds binary operator and signs to a number or None */
  private def foldBinOp(op: BinaryOperator, lhs: Sign, rhs: Sign): Option[Int] = (op, lhs, rhs) match {
    case (Equal, FlatVal(Zer), FlatVal(Zer)) => Some(1)
    case (Equal, FlatVal(Zer), FlatVal(Pos)) => Some(0)
    case (Equal, FlatVal(Zer), FlatVal(Neg)) => Some(0)
    case (Equal, FlatVal(Pos), FlatVal(Zer)) => Some(0)
    case (Equal, FlatVal(Pos), FlatVal(Neg)) => Some(0)
    case (Equal, FlatVal(Neg), FlatVal(Pos)) => Some(0)
    case (Equal, FlatVal(Neg), FlatVal(Zer)) => Some(0)
    case (GreatThan, FlatVal(Pos), FlatVal(Zer)) => Some(1)
    case (GreatThan, FlatVal(Pos), FlatVal(Neg)) => Some(1)
    case (GreatThan, FlatVal(Zer), FlatVal(Neg)) => Some(1)
    case (GreatThan, FlatVal(Zer), FlatVal(Pos)) => Some(0)
    case (GreatThan, FlatVal(Neg), FlatVal(Pos)) => Some(0)
    case (GreatThan, FlatVal(Neg), FlatVal(Zer)) => Some(0)
    case _ => None
  }

  /** Returns sign of a declaration based on the predecessors of the node */
  private def getSign(decl: Decl, stmt: CfgStmtNode): Sign = {
    stmt.pred
      .map(node => analyses.signs.get(node).getOrElse(decl, FlatTop)) //map predecessor to their constant value of a decl
      .reduceOption((l, r) => if (l != r) FlatTop else l)
      .getOrElse(FlatTop)
  }
}
