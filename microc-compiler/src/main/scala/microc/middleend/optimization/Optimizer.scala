package microc.middleend.optimization

import microc.frontend.ast._
import microc.middleend.AnalysesDb
import microc.middleend.analysis.{AnalysisHandlerInterface, Declarations, Types}
import microc.middleend.cfg.{BasicContext, BranchContext, CfgNode, CfgNodeContexts, CfgStmtNode, DoWhileContext, IfContext, ProgramCfg, WhileContext}

import scala.annotation.tailrec

/**
 * Optimizer that performs various optimization on the input CFG IR of the program
 * @param cfg CFG IR of the program
 * @param handler interface that defines methods for retrieving results of analyses
 * @param budget budget of the optimizer (each optimization has a cost which is deducted from the budget when its performed)
 */
class Optimizer(cfg: ProgramCfg, handler: AnalysisHandlerInterface, budget: Int = 0) {
  private var currBudget: Int = budget
  private var types: Types = handler.getTypes(cfg.program)
  private var decls: Declarations = handler.getDeclarations(cfg.program)

  @tailrec
  final def run(): AnalysesDb = {
    val analyses = AnalysesDb(
      types,
      decls,
      handler.getSigns(cfg)(decls),
      handler.getConstants(cfg)(decls),
      handler.getLiveVars(cfg)(decls),
      handler.getAvailableExps(cfg)(decls)
    )
    //perform one optimization pass
    val fixedPoint = !optimizeCfg(analyses)

    if (fixedPoint) {
      analyses
    } else {
      run()
    }
  }

  /** Performs one optimization pass on CFG and returns true if some optimizations were performed during this pass */
  def optimizeCfg(analyses: AnalysesDb): Boolean = {
    //find first optimization, that returns at least one optimization action
    for {
      optimization <- optimizationPlan(analyses) //for each optimization from optimization plan
      if optimization.isRunnable //that is runnable
      if optimization.cost < currBudget //and its cost doesn't exceed current budget
    } {
      val actions = optimization.run(cfg)
      currBudget -= optimization.cost
      if (actions.nonEmpty) {
        //perform derived optimization actions and end current optimization pass
        doOptimActions(actions, cfg.contexts)
        return true
      }
    }

    false
  }

  /** Performs all optimization actions on CFG */
  def doOptimActions(actions: List[OptimizationAction], contexts: CfgNodeContexts): Unit = {
    actions.foreach {
      case DeleteNode(stmt) =>
        val pred = stmt.pred.clone()
        val succ = stmt.succ.clone()
        deleteContext(stmt, contexts)
        disconnectNode(stmt)
        //connect predecessors to successors
        pred.foreach(p => succ.foreach(s => {
          p.succ.add(s)
          s.pred.add(p)
        }))

      case ReplaceNode(search, replace, stmt) =>
        //create a new node with replaced Expr
        val newStmt = CfgStmtNode(replaceAst(stmt.ast, search, replace))
        replaceNode(stmt, newStmt, contexts)

      case ConnectNodes(src, dest) =>
        src.succ += dest
        dest.pred += src

      case DisconnectNode(stmt, keepContexts) =>
        if (!keepContexts) {
          deleteContext(stmt, contexts)
        }
        disconnectNode(stmt)

      case AddDeclaration(idDecl, tp, usages, stmt) =>
        val varStmt = extractVarStmt(stmt)
        val vars = varStmt.decls :+ idDecl
        val newStmt = CfgStmtNode(VarStmt(vars, varStmt.loc))
        replaceNode(stmt, newStmt, contexts)
        //update Types
        types = types + (idDecl -> tp)
        //update Declarations
        decls = usages.foldLeft(decls)((acc, id) => acc + (id -> idDecl))

      case DeleteDeclaration(idDecl, stmt) =>
        val varStmt = extractVarStmt(stmt)
        val vars = varStmt.decls.filter(_.name != idDecl.name)
        val newStmt = CfgStmtNode(VarStmt(vars, varStmt.loc))
        replaceNode(stmt, newStmt, contexts)
        //update Types
        types = types.filter(_._1 != idDecl)
        //update Declarations
        decls = decls.filter(_._2 != idDecl)

      case PrependNode(stmt, context, location) =>
        val pred = location.pred.clone()
        //only predecessor of location is going to be stmt
        location.pred.clear()
        location.pred += stmt
        //connect stmt and location
        stmt.succ += location
        //connect stmt to previous predecessors of location
        pred.foreach(p => {
          stmt.pred += p //connect stmt to p
          p.succ -= location //disconnect p from location
          p.succ += stmt //connect p to stmt
        })
        //add context for the newly created cfg node
        contexts += stmt -> context

      case ChangeContext(context, stmt) =>
        contexts += (stmt -> context)
    }
  }

  /** Returns sequence of all available optimizations (aka "optimization plan) */
  private def optimizationPlan(analyses: AnalysesDb): List[Optimization] = List(
    new UnusedVarElimination(),
    new DeadStmtElimination(analyses),
    new BasicDeadCodeElimination(),
    new ConstantBasedDeadCodeElimination(analyses),
    new SignBasedDeadCodeElimination(analyses),
    new ConstantFolding(),
    new ConstantPropagation(analyses),
    new SignFolding(analyses),
    new CommonSubexpElimination(analyses),
  )

  /** Disconnects node from its successors and predecessors and removes it from contexts */
  private def disconnectNode(stmt: CfgNode): Unit = {
    //remove stmt from its predecessors and vice-versa
    stmt.pred.foreach(p => {
      p.succ.remove(stmt)
      stmt.pred.remove(p)
    })
    //remove stmt from its successors and vice-versa
    stmt.succ.foreach(s => {
      s.pred.remove(stmt)
      stmt.succ.remove(s)
    })
  }

  /** Replaces one CfgNode with another one in CFG, that will have the same successors and predecessors */
  private def replaceNode(oldStmt: CfgStmtNode, newStmt: CfgStmtNode, contexts: CfgNodeContexts): Unit = {
    val pred = oldStmt.pred.clone()
    val succ = oldStmt.succ.clone()
    //add context for the newly created cfg node
    contexts += newStmt -> contexts(oldStmt)
    disconnectNode(oldStmt)
    //delete context of the old stmt
    contexts -= oldStmt
    //replace all occurrences of the deleted node with the newly created node
    updateContexts(oldStmt, newStmt, contexts)
    //connect the new node
    pred.foreach(p => {
      p.succ.add(newStmt) //add new stmt
      newStmt.pred.add(p) //add predecessor to new stmt
    })
    succ.foreach(s => {
      s.pred.add(newStmt) //add new stmt
      newStmt.succ.add(s) //add successor to new stmt
    })
  }

  /** Replaces all occurrences of `search` with `replace` in `ast` and returns modified AST node */
  private def replaceAst(ast: AstNode, search: AstNode, replace: AstNode): AstNode = (search, replace) match {
    case _ if ast == search => replace
    //only expressions can be replaced inside a statement or an expression
    case (searchExpr: Expr, replaceExpr: Expr) => replaceStmt(ast, searchExpr, replaceExpr)
    case _ => ast
  }

  /** Replaces all occurrences of `search` with `replace` in `ast` and returns modified AST statement */
  private def replaceStmt(ast: AstNode, search: Expr, replace: Expr): AstNode = ast match {
    case expr: Expr =>
      replaceExpr(expr, search, replace)

    case AssignStmt(lhs, rhs, loc) =>
      AssignStmt(replaceExpr(lhs, search, replace), replaceExpr(rhs, search, replace), loc)

    case ReturnStmt(expr, loc) =>
      ReturnStmt(replaceExpr(expr, search, replace), loc)

    case OutputStmt(expr, loc) =>
      OutputStmt(replaceExpr(expr, search, replace), loc)

    case _ => ast
  }

  /** Replaces all occurrences of `search` with `replace` in `expr` and returns modified Expr */
  private def replaceExpr(expr: Expr, search: Expr, replace: Expr): Expr = expr match {
    case _ if expr == search => replace

    case Deref(expr, loc) =>
      Deref(replaceExpr(expr, search, replace), loc)

    case BinaryOp(op, lhs, rhs, loc) =>
      BinaryOp(op, replaceExpr(lhs, search, replace), replaceExpr(rhs, search, replace), loc)

    case CallFuncExpr(targetFun, args, loc) =>
      val newFun = replaceExpr(targetFun, search, replace)
      val newArgs = args.map(e => replaceExpr(e, search, replace))
      CallFuncExpr(newFun, newArgs, loc)

    case Alloc(expr, loc) =>
      Alloc(replaceExpr(expr, search, replace), loc)

    case VarRef(id, loc) =>
      val newId = replaceExpr(id, search, replace).asInstanceOf[Identifier]
      VarRef(newId, loc)

    case Record(fields, loc) =>
      val newFields = fields.map(f => RecordField(f.name, replaceExpr(f.expr, search, replace), f.loc))
      Record(newFields, loc)

    case FieldAccess(record, field, loc) =>
      FieldAccess(replaceExpr(record, search, replace), field, loc)

    case Array(elems, loc) =>
      val newElems = elems.map(replaceExpr(_, search, replace))
      Array(newElems, loc)

    case ArrayAccess(array, index, loc) =>
      ArrayAccess(replaceExpr(array, search, replace), replaceExpr(index, search, replace), loc)

    case _ => expr
  }

  /** Replaces all occurrences of `search` with `replace` in every cfg context */
  private def updateContexts(search: CfgNode, replace: CfgNode, contexts: CfgNodeContexts): Unit = {
    contexts.foreach(item => item._2 match {
      case ifContext: IfContext =>
        val thenBranch = updateBranchContext(search, replace, ifContext.thenBranch)
        val elseBranch = updateBranchContext(search, replace, ifContext.elseBranch)
        contexts += item._1 -> IfContext(thenBranch, elseBranch)

      case whileContext: WhileContext =>
        val bodyBranch = updateBranchContext(search, replace, whileContext.bodyBranch)
        contexts += item._1 -> WhileContext(bodyBranch)

      case doWhileContext: DoWhileContext =>
        val bodyBranch = updateBranchContext(search, replace, doWhileContext.bodyBranch)
        val guard = if (search == doWhileContext.guard) replace else doWhileContext.guard
        contexts += item._1 -> DoWhileContext(bodyBranch, guard)

      case BasicContext => ()
    })
  }

  /** Replaces all occurrences of `search` with `replace` in the branch definition */
  private def updateBranchContext(search: CfgNode, replace: CfgNode, branch: Option[BranchContext]): Option[BranchContext] = {
    branch.map(b => {
      val entry = if (b.entry == search) replace else b.entry
      val exit = if (b.exit.contains(search)) (b.exit - search) + replace else b.exit
      BranchContext(entry, exit)
    })
  }

  private def extractVarStmt(stmt: CfgStmtNode): VarStmt = stmt.ast match {
    case vs: VarStmt => vs
    case _ => throw new RuntimeException("Incorrect usage of optimization action, no VarStmt found")
  }

  /** Deletes node from contexts and all of its usages in contexts */
  private def deleteContext(stmt: CfgNode, contexts: CfgNodeContexts): Unit = {
    contexts -= stmt
    //delete usage of the node in each context
    contexts.foreach(x => x._2 match {
      case ifContext: IfContext =>
        val thenBranch = deleteBranchContext(stmt, ifContext.thenBranch)
        val elseBranch = deleteBranchContext(stmt, ifContext.elseBranch)
        contexts += x._1 -> IfContext(thenBranch, elseBranch)

      case whileContext: WhileContext =>
        contexts += x._1 -> WhileContext(deleteBranchContext(stmt, whileContext.bodyBranch))

      case doWhileContext: DoWhileContext if doWhileContext.guard == stmt =>
        contexts += x._1 -> BasicContext

      case doWhileContext: DoWhileContext =>
        contexts += x._1 -> DoWhileContext(deleteBranchContext(stmt, doWhileContext.bodyBranch), doWhileContext.guard)

      case BasicContext => ()
    })
  }

  /** Deletes usage of a node inside of a branch */
  private def deleteBranchContext(stmt: CfgNode, branch: Option[BranchContext]): Option[BranchContext] = {
    branch.foldLeft(Option.empty[BranchContext])((_, b) => (b.entry == stmt, b.exit.contains(stmt)) match {
      //node is used as an entry - its successor will become a new entry
      case (true, false) =>
        val newEntry = (stmt.succ - stmt).head
        Some(BranchContext(newEntry, b.exit))

      //node is used as an exit - predecessors of the node will become exits instead
      case (false, true) =>
        val newExit = b.exit.union(stmt.pred) - stmt
        Some(BranchContext(b.entry, newExit))

      //node is not used anywhere, no need to change the context
      case (false, false) => branch

      //node is both entry and exit, deleting it will leave an empty branch
      case (true, true) => None
    })
  }
}
