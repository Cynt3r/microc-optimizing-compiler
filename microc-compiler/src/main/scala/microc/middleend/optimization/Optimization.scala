package microc.middleend.optimization

import microc.frontend.ast.{ArrayAccess, AssignStmt, AstNode, BinaryOperator, CallFuncExpr, Divide, Equal, GreatThan, Input, Minus, Number, OutputStmt, Plus, Times}
import microc.middleend.cfg.{BasicContext, BranchContext, CfgStmtNode, DoWhileContext, ProgramCfg}

/** Optimization that takes analyses and produces sequence of optimization actions */
trait Optimization {

  /** Cost of the optimization */
  val cost: Int

  /** Returns true, if optimization is runnable (some optimization may require various analysis results to be able to run) */
  def isRunnable: Boolean

  /** Derives all possible optimization actions from the input CFG */
  def run(cfg: ProgramCfg): List[OptimizationAction]


  //HELPERS:

  /** Returns all CfgStmtNodes of a ProgramCfg */
  protected def collectCfgStmts(cfg: ProgramCfg): List[CfgStmtNode] = {
    cfg.nodes
      .toList
      .flatMap {
        case stmt: CfgStmtNode => Some(stmt)
        case _ => None
      }
  }

  /** Checks if AST contains Expr that causes side-effects (input, call) */
  protected def hasSideEffect(ast: AstNode): Boolean = {
    !ast.tree.forall {
      case _: Input => false //read from stdin
      case _: OutputStmt => false //write to stdout
      case _: CallFuncExpr => false //call of function that contains a side-effect expression
      case _: ArrayAccess => false //out of bounds access
      case AssignStmt(lhs, _, _) => lhs match {
        case ArrayAccess(_, _, _) => false
        case _ => true //out of bounds access
      }
      case _ => true
    }
  }

  /** Folds binary operator and numbers */
  protected def foldBinOp(op: BinaryOperator, lVal: Int, rVal: Int): Int = op match {
    case Plus => lVal + rVal
    case Minus => lVal - rVal
    case Times => lVal * rVal
    case Divide => lVal / rVal
    case Equal => if (lVal == rVal) 1 else 0
    case GreatThan => if (lVal > rVal) 1 else 0
  }

  /** Generates optimization actions that disconnect a branch from the CFG */
  protected def disconnectBranch(branch: Option[BranchContext]): List[DisconnectNode] = {
    branch.map(b => {
      b.exit.foldLeft(Set(DisconnectNode(b.entry.asInstanceOf[CfgStmtNode], keepContexts = false)))((acc, node) => {
        acc + DisconnectNode(node.asInstanceOf[CfgStmtNode], keepContexts = false)
      })
    }).getOrElse(Set()).toList
  }

  /** Generates optimization actions that transform while to do-while statement */
  protected def whileToDoWhile(guard: CfgStmtNode, bodyBranch: BranchContext): List[OptimizationAction] = {
    //disconnect the guard
    val disconnectNode = DisconnectNode(guard, keepContexts = true)
    val origEntry = bodyBranch.entry.asInstanceOf[CfgStmtNode]
    //create a "dull" (NOP) CFG node, that will become the new entry (we need CFG node with no context as an entry)
    val newEntry = CfgStmtNode(Number(0, origEntry.ast.loc))
    val newContext = DoWhileContext(Some(BranchContext(newEntry, bodyBranch.exit)), guard)
    val prependActions = List(PrependNode(newEntry, newContext, origEntry))

    //connect predecessors of the guard (except for exit nodes of the body) to entry of the body
    val entryConnectNodes = guard.pred
      .diff(bodyBranch.exit)
      .map(n => ConnectNodes(n.asInstanceOf[CfgStmtNode], newEntry))
      .toList
    //connect exit nodes of the body to guard
    val guardEntryConnect = bodyBranch.exit.map(n => ConnectNodes(n.asInstanceOf[CfgStmtNode], guard)).toList
    //connect guard to entry node of the body
    val guardLoop = ConnectNodes(guard, newEntry)
    //connect guard to nodes after while-statement
    val guardFinally = guard.succ
      .filterNot(_ == bodyBranch.entry)
      .map(n => ConnectNodes(guard, n.asInstanceOf[CfgStmtNode]))
      .toList
    //replace context of the guard
    val guardContext = ChangeContext(BasicContext, guard)
    disconnectNode :: prependActions ::: entryConnectNodes ::: guardEntryConnect ::: guardFinally ::: List(guardLoop, guardContext)
  }
}
