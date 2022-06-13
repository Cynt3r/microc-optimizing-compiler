package microc.middleend.optimization
import microc.frontend.ast._
import microc.middleend.ProgramException
import microc.middleend.cfg.{CfgStmtNode, ProgramCfg}

/** Optimization that performs constant folding of binary operations */
class ConstantFolding() extends Optimization {
  val cost: Int = 1

  //always runnable, since no analysis is required
  def isRunnable: Boolean = true

  def run(cfg: ProgramCfg): List[OptimizationAction] = {
    collectCfgStmts(cfg)
      .flatMap(stmt => {
        //collect optimization from each sub-ast-node of a stmt
        stmt.ast.tree.flatMap(ast => collectFromAst(ast, stmt))
      })
  }

  private def collectFromAst(ast: AstNode, stmt: CfgStmtNode): List[OptimizationAction] = ast match {
    //0 + EXPR = EXPR
    case binOp @ BinaryOp(Plus, Number(0, _), e, _) =>
      List(ReplaceNode(binOp, e, stmt))

    //EXPR + 0 = EXPR
    case binOp @ BinaryOp(Plus, e, Number(0, _), _) =>
      List(ReplaceNode(binOp, e, stmt))

    //EXPR - 0 = EXPR
    case binOp @ BinaryOp(Minus, e, Number(0, _), _) =>
      List(ReplaceNode(binOp, e, stmt))

    //ID - ID = 0
    case binOp @ BinaryOp(Minus, Identifier(n1, l), Identifier(n2, _), _) if n1 == n2 =>
      List(ReplaceNode(binOp, Number(0, l), stmt))

    //ID * 0 = 0
    case binOp @ BinaryOp(Times, Identifier(_, l), Number(0, _), _) =>
      List(ReplaceNode(binOp, Number(0, l), stmt))

    //0 * ID = 0
    case binOp @ BinaryOp(Times, Number(0, l), Identifier(_, _), _) =>
      List(ReplaceNode(binOp, Number(0, l), stmt))

    //EXPR * 1 = EXPR
    case binOp @ BinaryOp(Times, e, Number(1, _), _) =>
      List(ReplaceNode(binOp, e, stmt))

    //1 * EXPR = EXPR
    case binOp @ BinaryOp(Times, Number(1, _), e, _) =>
      List(ReplaceNode(binOp, e, stmt))

    //EXPR / 0 = exception
    case binOp @ BinaryOp(Divide, _, Number(0, _), _) =>
      throw new ProgramException(s"Dividing by zero intercepted at [${binOp.loc}]")

    //EXPR / 1 = EXPR
    case binOp @ BinaryOp(Divide, e, Number(1, _), _) =>
      List(ReplaceNode(binOp, e, stmt))

    //num op num = num
    case binOp @ BinaryOp(op, Number(lVal, _), Number(rVal, _), loc) =>
      val res = foldBinOp(op, lVal, rVal)
      val replacement = Number(res, loc)
      List(ReplaceNode(binOp, replacement, stmt))

    case _ =>
      List()
  }
}
