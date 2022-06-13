package microc.middleend.optimization

import microc.frontend.ast.{Alloc, AssignStmt, AstNode, BinaryOp, CallFuncExpr, Deref, Equal, Expr, FieldAccess, Identifier, IdentifierDecl, Input, IntType, Loc, Null, Number, Plus, Record, Times, VarRef, VarStmt}
import microc.middleend.AnalysesDb
import microc.middleend.cfg.{BasicContext, CfgStmtNode, FunctionCfg, ProgramCfg}

import scala.annotation.tailrec

/** Represents usage of a Expr in CfgStmtNode */
case class ExpUsage(expr: Expr, stmt: CfgStmtNode)

/** Optimization that eliminates common expressions and replaces them with a temporary variable */
class CommonSubexpElimination(analyses: AnalysesDb) extends Optimization {
  /** Counter for fresh variables */
  private var tempCnt: Int = 0

  val cost: Int = 2

  //available expression analysis is required
  def isRunnable: Boolean = analyses.availableExps.nonEmpty

  def run(cfg: ProgramCfg): List[OptimizationAction] = {
    cfg.functionsCfg
      .toList
      .flatMap(collectFromFun)
  }

  private def collectFromFun(fun: FunctionCfg): List[OptimizationAction] = {
    //collect all expressions
    val binOps = fun.nodes
      .flatMap(cfgNode => analyses.availableExps.get(cfgNode)) //collect all expressions
      .flatMap { //filter binary operations only (only those are worth eliminating)
        case binOp: BinaryOp => Some(binOp)
        case _ => None
      }
      .map(binOp => (binOp, rankBinOp(binOp))) //assign a rank to each expr based on number of nested binary operations
      .toList
      .sortBy(_._2)(Ordering[Int].reverse) //sort by rank DESC
      .map(_._1) //get rid of ranks since they will be no longer needed

    findCommonSubexp(binOps, fun) match {
      case Some((binOp, usages)) =>
        val decl = getFreshDecl
        val prependAction = createPrependAction(decl, binOp, usages)
        val replaceActions = usages.map(u => ReplaceNode(u.expr, Identifier(decl.name, u.expr.loc), u.stmt))
        //retrieve all usages of a new declaration from previously created replace and prepend actions
        val prependDeclUsage = prependAction.stmt.ast.asInstanceOf[AssignStmt].left.asInstanceOf[Identifier]
        val replaceDeclUsages = replaceActions.map(_.replace.asInstanceOf[Identifier])
        val declUsages = prependDeclUsage :: replaceDeclUsages
        val addDeclAction = AddDeclaration(decl, IntType, declUsages, getVarStmtNode(fun))
        addDeclAction :: prependAction :: replaceActions


      //no binary operation is reusable more than once -> no optimization
      case None => List()
    }
  }

  /** Finds binary operation that is reused in multiple places */
  @tailrec
  private def findCommonSubexp(binOps: List[BinaryOp], fun: FunctionCfg): Option[(BinaryOp, List[ExpUsage])] = binOps match {
    case binOp :: rest =>
      //filter CfgStmtNode that have available binOp
      val stmts = fun.nodes .flatMap {
          case stmt: CfgStmtNode if isBinopAvailable(binOp, stmt) => Some(stmt)
          case _ => None
        }
      //find usages in each stmt
      val usages = stmts.foldLeft(List.empty[ExpUsage])((acc, stmt) => {
        val exprUsages = usagesInAst(binOp, stmt.ast)
        //map exprs to their occurrences in CFG nodes
        acc ::: exprUsages.map(expr => ExpUsage(expr, stmt))
      })


      if (usages.size > 1) { //only expressions with two ore more occurrences are worth optimizing
        Some(binOp, usages)
      } else { //try another expression in the list
        findCommonSubexp(rest, fun)
      }

    case Nil => None
  }

  /** Finds all usages of target expression in the ast node */
  private def usagesInAst(target: Expr, ast: AstNode): List[Expr] = {
    ast.tree
      .flatMap { //filter all expressions
        case expr: Expr => Some(expr)
        case _ => None
      }
      .filter(expr => isSameExpr(target, expr)) //filter all usages
      .toList
  }

  /** Returns true if two expression have the same shape (location can differ) */
  private def isSameExpr(lhs: Expr, rhs: Expr): Boolean = (lhs, rhs) match {
    case (_: Null, _: Null) => true
    case (Number(n1, _), Number(n2, _)) if n1 == n2 => true
    case (Identifier(n1, _), Identifier(n2, _)) if n1 == n2 => true
    case (Deref(e1, _), Deref(e2, _)) => isSameExpr(e1, e2)
    case (BinaryOp(o1, l1, r1, _), BinaryOp(o2, l2, r2, _)) if o1 == o2 =>
      o1 match {
        //commutative operations
        case Plus | Times | Equal =>
          (isSameExpr(l1, l2) && isSameExpr(r1, r2)) || (isSameExpr(l1, r2) && isSameExpr(r1, l2))
        //non-commutative operations
        case _ =>
          isSameExpr(l1, l2) && isSameExpr(r1, r2)
      }
    case (CallFuncExpr(f1, a1, _), CallFuncExpr(f2, a2, _)) if a1.size == a2.size =>
      isSameExpr(f1, f2) && a1.zip(a2).forall(x => isSameExpr(x._1, x._2))
    case (_: Input, _: Input) => true
    case (Alloc(e1, _), Alloc(e2, _)) => isSameExpr(e1, e2)
    case (VarRef(i1, _), VarRef(i2, _)) => isSameExpr(i1, i2)
    case (Record(f1, _), Record(f2, _)) if f1.size == f2.size =>
      f1.zip(f2).forall(x => x._1.name == x._2.name && isSameExpr(x._1.expr, x._2.expr))
    case (FieldAccess(r1, f1, _), FieldAccess(r2, f2, _)) if f1 == f2 => isSameExpr(r1, r2)
    case _ => false
  }

  /** Ranks binary operator based on the number of nested binary operators */
  private def rankBinOp(binOp: BinaryOp): Int = binOp.tree.count(_.isInstanceOf[BinaryOp])

  private def getVarStmtNode(fun: FunctionCfg): CfgStmtNode = {
    fun.nodes
      .collectFirst {
        case stmt @ CfgStmtNode(VarStmt(_, _)) => stmt
      }
      .getOrElse(throw new RuntimeException(s"Cfg of function ${fun.fun.name} is missing VarStmt"))
  }

  /** Creates prepend action that contains assignment to a new temporary variable */
  private def createPrependAction(decl: IdentifierDecl, expr: Expr, usages: List[ExpUsage]): PrependNode = {
    //find the first usage of a expression, that will be eliminated
    val firstUsage = usages
      .find(u => u.expr.tree.toSet.contains(expr))
      .getOrElse(throw new RuntimeException(s"Inconsistency in available expression analysis, $expr is not declared available in its first occurrence in CFG"))
    val firsUsageLoc = firstUsage.stmt.ast.loc
    //create a new CFG node that will assign expr to newly created temporary variable
    val newNode = CfgStmtNode(AssignStmt(
      Identifier(decl.name, Loc(firsUsageLoc.line, firsUsageLoc.col + 1)),
      expr,
      Loc(firsUsageLoc.line, firsUsageLoc.col)
    ))
    PrependNode(newNode, BasicContext, firstUsage.stmt)
  }

  /** Returns fresh variable declaration */
  @tailrec
  private def getFreshDecl: IdentifierDecl = {
    val id = tempCnt
    tempCnt += 1
    val declName = "t" + id
    if (analyses.declarations.exists(_._2.name == declName)) { //declaration already in use -> try again
      getFreshDecl
    } else {
      IdentifierDecl(declName, Loc(0, 0))
    }
  }

  /** Returns true if the binary operation is available at the given CFG node */
  private def isBinopAvailable(binOp: BinaryOp, stmt: CfgStmtNode): Boolean = {
    if (analyses.availableExps.get(stmt).contains(binOp)) { //either is the binOp available after the stmt node
      true
    } else { //or its available in every predecessor of the stmt node
      stmt.pred
        .map(node => analyses.availableExps.get(node)) //get all available expression of predecessors of the stmt
        .forall(_.contains(binOp)) //every predecessor must have available the given binOp
    }
  }
}
