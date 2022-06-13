package microc.middleend.cfg

import microc.frontend.ast._
import microc.middleend.ProgramException

import scala.annotation.tailrec
import scala.collection.mutable

case class CfgConstructionException(message: String, loc: Loc) extends ProgramException(message + s" [$loc]")

/**
  * A CFG fragment.
  * This can be single statement, a sequence of statements, a complete function or a complete program.
  */
class FragmentCfg(val entryNodes: Set[CfgNode], val exitNodes: Set[CfgNode])(implicit val contexts: CfgNodeContexts) {

  /** Returns true if this is an empty CFG */
  def isEmpty: Boolean = entryNodes.isEmpty && exitNodes.isEmpty

  /** Returns a new CFG that is the concatenation of this CFG with `that` CFG.
    */
  def ~(that: FragmentCfg): FragmentCfg =
    if (isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      this.exitNodes.foreach(_.succ ++= that.entryNodes)
      that.entryNodes.foreach(_.pred ++= this.exitNodes)
      new FragmentCfg(this.entryNodes, that.exitNodes)(contexts.merge(that.contexts))
    }

  /** Returns a new CFG that is the union of this CFG with `that`. */
  def |(that: FragmentCfg): FragmentCfg =
    new FragmentCfg(that.entryNodes.union(entryNodes), that.exitNodes.union(exitNodes))(contexts.merge(that.contexts))

  /** Returns the set of nodes in the CFG. */
  def nodes: Set[CfgNode] = entryNodes.flatMap(visit)

  protected def visit(n: CfgNode): Set[CfgNode] = {
    @tailrec
    def visit0(rem: List[CfgNode], visited: Set[CfgNode]): Set[CfgNode] = rem match {
      case Nil => visited
      case x :: xs if visited(x) => visit0(xs, visited)
      case x :: xs => visit0(xs ++ x.succ, visited + x)
    }

    visit0(List(n), Set())
  }
}

class FunctionCfg(val fun: FunDecl, val entry: CfgFunEntryNode, val exit: CfgFunExitNode, val cfg: FragmentCfg)(implicit contexts: CfgNodeContexts)
    extends FragmentCfg(Set(entry), Set(exit))

class ProgramCfg(val program: Program, val functionsCfg: Set[FunctionCfg])(implicit contexts: CfgNodeContexts)
    extends FragmentCfg(functionsCfg.map(_.entry), functionsCfg.map(_.exit))

trait CfgFactory {
  def fromAstNode(n: AstNode): FragmentCfg

  def fromFunction(f: FunDecl): FunctionCfg

  def fromProgram(p: Program): ProgramCfg = {
    CfgNode.nextUidCounter.set(0)
    val functionsCfg = p.funs.map(fromFunction).toSet
    //merge contexts of all functions into one
    implicit val contexts: CfgNodeContexts = functionsCfg.foldLeft(mutable.Map.empty[CfgNode, CfgNodeContext])((contexts, fun) => {
      contexts.merge(fun.contexts)
    })
    new ProgramCfg(p, functionsCfg)
  }
}

class IntraproceduralCfgFactory extends CfgFactory {
  private def empty(): FragmentCfg = new FragmentCfg(Set(), Set())(mutable.Map())

  private def single(node: CfgNode, context: CfgNodeContext) = new FragmentCfg(Set(node), Set(node))(mutable.Map(node -> context))

  def fromAstNode(node: AstNode): FragmentCfg =
    node match {
      case _: AssignStmt =>
        single(CfgStmtNode(ast = node), BasicContext)

      case block: Block =>
        block.body.foldLeft(empty())((acc, stmt) => acc ~ fromAstNode(stmt))

      case IfStmt(guard, thenBranch, elseBranch, _) =>
        val thenCfg = fromAstNode(thenBranch)
        val elseCfg = elseBranch.map(x => fromAstNode(x))

        //create context for if statement
        val thenContext = thenCfg.entryNodes match {
          case e if e.nonEmpty => Some(BranchContext(thenCfg.entryNodes.head, thenCfg.exitNodes))
          case _ => None
        }
        val elseContext = elseCfg match {
          case Some(e) if e.entryNodes.nonEmpty => Some(BranchContext(e.entryNodes.head, e.exitNodes))
          case _ => None
        }
        val ifContext = IfContext(thenContext, elseContext)

        val guardCfg = single(CfgStmtNode(ast = guard), ifContext)
        val guardedThenCfg = guardCfg ~ thenCfg
        val guardedElseCfg = elseCfg.map(x => guardCfg ~ x)
        guardedElseCfg.fold(guardedThenCfg | guardCfg)(x => guardedThenCfg | x)

      case _: OutputStmt =>
        single(CfgStmtNode(ast = node), BasicContext)

      case _: ReturnStmt =>
        single(CfgStmtNode(ast = node), BasicContext)

      case _: VarStmt =>
        single(CfgStmtNode(ast = node), BasicContext)

      case WhileStmt(guard, block, _) =>
        val blockCfg = fromAstNode(block)

        //create context for while statement
        val blockContext = blockCfg.entryNodes match {
          case e if e.nonEmpty => Some(BranchContext(e.head, blockCfg.exitNodes))
          case _ => None
        }
        val whileContext = WhileContext(blockContext)

        val guardCfg = single(CfgStmtNode(ast = guard), whileContext)
        val loopCfg = guardCfg ~ blockCfg ~ guardCfg
        loopCfg | guardCfg

      case x =>
        throw CfgConstructionException(s"Unsupported AST node '$node'", x.loc)
    }

  def fromFunction(fun: FunDecl): FunctionCfg = {
    val entry = CfgFunEntryNode(ast = fun)
    val blockCfg = fromAstNode(fun.block)
    val exit = CfgFunExitNode(ast = fun)
    val cfg = single(entry, BasicContext) ~ blockCfg ~ single(exit, BasicContext)

    new FunctionCfg(fun, entry, exit, cfg)(blockCfg.contexts)
  }
}
