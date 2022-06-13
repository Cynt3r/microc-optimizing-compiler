package microc.backend.x86IR

import microc.frontend.ast.{Array, ArrayAccess, ArrayWrite, AssignStmt, AstNode, BinaryOp, CallFuncExpr, Deref, DirectFieldWrite, DirectWrite, Equal, Expr, FieldAccess, GreatThan, Identifier, IndirectFieldWrite, IndirectWrite, Input, Loc, Null, Number, OutputStmt, Record, ReturnStmt, VarRef, VarStmt, Alloc => AstAlloc, Divide => AstDivide, Minus => AstMinus, Plus => AstPlus, Times => AstTimes}
import microc.middleend.analysis.{Declarations, Types}
import microc.middleend.cfg.{BasicContext, BranchContext, CfgFunEntryNode, CfgFunExitNode, CfgNode, CfgNodeContext, CfgNodeContexts, CfgStmtNode, DoWhileContext, IfContext, ProgramCfg, WhileContext}

/**
 * Compiler that takes CFG IR and produces x86 IR
 * @param cfg CFG IR of the program
 * @param types result of the type analysis of the program
 * @param decls result of the declaration analysis of the program
 */
class CfgCompiler(cfg: ProgramCfg, types: Types, decls: Declarations) {
  val builder: X86IRBuilder = new X86IRBuilder()
  val tAnalyzer: TypeAnalyzer = new TypeAnalyzer(types, decls)

  /** Compiles CFG IR to x86 IR */
  def compile(): ProgramX86IR = {
    //visit every function
    cfg.program.funs
      .map(_.name) //get list of functions in order they were declared
      .flatMap(name => cfg.functionsCfg.find(_.fun.name == name)) //map them to their function CFG
      .foreach(fun => visitCfg(fun.entryNodes.head, List())(cfg.contexts)) //visit each function CFG
    builder.build()
  }

  /** Compiles CfgNode */
  private def visitCfg(node: CfgNode, contextStack: List[CfgNodeContext])(implicit contexts: CfgNodeContexts): Unit = node match {
    case CfgFunEntryNode(ast) =>
      val funAddr = FunAlloc(ast, ast.loc)
      builder.enterFunction(funAddr)
      //register each param of the function
      ast.params.foreach(param => {
        val tp = tAnalyzer.declIRType(param)
        val alloc = Alloc(param, PointType(tp), param.loc)
        builder.addInstruction(ArgAddr(alloc, param.loc), node)
        builder.addVariable(alloc)
      })
      visitCfg(node.succ.head, contextStack)

    case _: CfgFunExitNode =>
      builder.leaveFunction()

    case stmt: CfgStmtNode =>
      //compilation of CfgStmtNode depends on its context; after compilation return the last compiled statement and following one
      val (lastStmt, nextStmt) = contexts(node) match {
        case BasicContext =>
          visitAst(stmt.ast, lvalue = false, stmt)
          (node, node.succ.head)

        case ifContext @ IfContext(thenBranch, elseBranch) =>
          val thenBb = builder.createBbNewSuffix("then")
          val elseBb = builder.createBbSameSuffix("else")
          val finallyBb = builder.createBbSameSuffix("finally")

          //compile guard
          visitGuard(stmt, thenBb, elseBb)

          //enter the basic block for then branch and compile it
          val thenLastStmt = visitBranch(thenBb, thenBranch, ifContext :: contextStack).getOrElse(stmt)
          builder.addInstruction(Jump(finallyBb.name, thenLastStmt.ast.loc), thenLastStmt)

          //enter the basic block for else branch and compile it
          val elseLastStmt = visitBranch(elseBb, elseBranch, ifContext :: contextStack).getOrElse(stmt)
          builder.addInstruction(Jump(finallyBb.name, elseLastStmt.ast.loc), elseLastStmt)

          //enter the basic block that comes after the then/else branches
          builder.enterBb(finallyBb)

          //get next statement to compile based on the context of the last statement of the if-else
          val nextStmt = contexts(thenLastStmt) match {
            case WhileContext(Some(b)) => (thenLastStmt.succ - b.entry).head
            case WhileContext(None) => (thenLastStmt.succ - thenLastStmt).head
            case DoWhileContext(Some(b), _) => (thenLastStmt.succ - b.entry).head
            case DoWhileContext(None, _) => (thenLastStmt.succ - thenLastStmt).head
            case _ => thenLastStmt.succ.head
          }
          //last compiled statement and the next, that will be compiled (single successor of the last statement)
          (thenLastStmt, nextStmt)

        case whileContext @ WhileContext(bodyBranch) =>
          val guardBb = builder.createBbNewSuffix("guard")
          val bodyBb = builder.createBbSameSuffix("body")
          val finallyBb = builder.createBbSameSuffix("finally")

          builder.addInstruction(Jump(guardBb.name, stmt.ast.loc), stmt)
          //enter the basic block for guard and compile the guard
          builder.enterBb(guardBb)
          visitGuard(stmt, bodyBb, finallyBb)

          //enter the basic block for while body and compile the body
          val bodyLastStmt = visitBranch(bodyBb, bodyBranch, whileContext :: contextStack).getOrElse(stmt)
          builder.addInstruction(Jump(guardBb.name, bodyLastStmt.ast.loc), bodyLastStmt)

          //enter the basic block that comes after the body branch
          builder.enterBb(finallyBb)
          //retrieve statement that will be compiled next
          val nextStmt = bodyBranch match {
            //guard has two successors - first stmt in body and first stmt that comes after the while - take the latter
            case Some(b) => (stmt.succ - b.entry).head
            //guard has two successor - guard and first stmt that comes after the while - take the latter
            case None => (stmt.succ - stmt).head
          }
          (stmt, nextStmt)

        case doWhileContext @ DoWhileContext(bodyBranch, guard) =>
          val bodyBb = builder.createBbNewSuffix("body")
          val guardBb = builder.createBbSameSuffix("guard")
          val finallyBb = builder.createBbSameSuffix("finally")

          builder.addInstruction(Jump(bodyBb.name, stmt.ast.loc), stmt)
          //enter the basic block
          builder.enterBb(bodyBb)
          visitAst(stmt.ast, lvalue = false, stmt)
          //if stmt isn't also an exit of the branch, continue with compilation of its only successor
          if (!doWhileContext.bodyBranch.get.exit.contains(stmt)) {
            visitCfg(stmt.succ.head, doWhileContext :: contextStack)
          }
          builder.addInstruction(Jump(guardBb.name, guard.ast.loc), stmt)

          //enter the basic block for guard and compile the guard
          builder.enterBb(guardBb)
          visitGuard(guard.asInstanceOf[CfgStmtNode], bodyBb, finallyBb)

          //enter the basic block that comes after the body branch
          builder.enterBb(finallyBb)
          val nextStmt = bodyBranch match {
            //guard has two successors - first stmt in body and first stmt that comes after the while - take the latter
            case Some(_) => (guard.succ - stmt).head
            //guard has two successor - guard and first stmt that comes after the while - take the latter
            case None => (guard.succ - guard).head
          }
          (guard, nextStmt)
      }

      if (continueCompilation(lastStmt, contextStack)) {
        //every cfg node with basic context has only one successor
        visitCfg(nextStmt, contextStack)
      }
  }

  /** Compiles guard from CfgStmtNode */
  private def visitGuard(stmt: CfgStmtNode, trueBb: X86BbBuilder, falseBb: X86BbBuilder): Unit = {
    val guard = visitAst(stmt.ast, lvalue = false, stmt).get
    builder.addInstruction(CondJump(guard, trueBb.name, falseBb.name, stmt.ast.loc), stmt)
  }

  /** Compiles branch of a if/else or while statement */
  private def visitBranch(branchBb: X86BbBuilder, branch: Option[BranchContext], contextStack: List[CfgNodeContext])(implicit contexts: CfgNodeContexts): Option[CfgStmtNode] = {
    builder.enterBb(branchBb)
    branch match {
      case Some(b) =>
        visitCfg(b.entry, contextStack)
        //get any of the exit nodes of the branch (which will always be a CfgStmtNode)
        Some(b.exit.head.asInstanceOf[CfgStmtNode])

      case None =>
        None
    }
  }

  /** Returns true if compilation of CfgNodes should continue based on current active context */
  private def continueCompilation(node: CfgNode, contextStack: List[CfgNodeContext]): Boolean = {
    //collect exit nodes based on context
    val exitNodes = contextStack match {
      case IfContext(thenBranch, elseBranch) :: _ =>
        val thenExit = thenBranch.foldLeft(Set.empty[CfgNode])((_, b) => b.exit)
        val elseExit = elseBranch.foldLeft(Set.empty[CfgNode])((_, b) => b.exit)
        thenExit.union(elseExit)

      case WhileContext(Some(bodyBranch)) :: _ =>
        bodyBranch.exit

      case DoWhileContext(Some(bodyBranch), _) :: _ =>
        bodyBranch.exit

      case _ =>
        Set.empty[CfgNode]
    }
    !exitNodes.contains(node)
  }

  /** Compiles AST node */
  private def visitAst(ast: AstNode, lvalue: Boolean, node: CfgStmtNode): Option[Instruction] = ast match {
    case VarStmt(decls, _) =>
      decls.foreach(decl => {
        val tp = tAnalyzer.declIRType(decl)
        val addr = Alloc(decl, PointType(tp), decl.loc)
        builder.addInstruction(addr, node)
        builder.addVariable(addr)
      })
      None

    case AssignStmt(left, right, loc) =>
      val addr = left match {
        case DirectWrite(id) =>
          visitAst(id, lvalue = true, node).get
        case IndirectWrite(expr) =>
          visitAst(expr, lvalue = true, node).get
        case DirectFieldWrite(id, field, dfwLoc) =>
          visitRecordAccess(id, recordLvalue = true, field, dfwLoc, node)
        case IndirectFieldWrite(expr, field, ifwLoc) =>
          visitRecordAccess(expr, recordLvalue = true, field, ifwLoc, node)
        case ArrayWrite(id, index, loc) =>
          visitArrayAccess(id, arrayLvalue = true, index, loc, node)
      }
      val value = visitAst(right, lvalue = false, node).get
      builder.addInstruction(Store(addr, value, loc), node)
      None

    case AstAlloc(expr, loc) =>
      val value = visitAst(expr, lvalue = false, node).get
      Some(builder.addInstruction(HeapAlloc(value, loc), node))

    case BinaryOp(operator, left, right, loc) =>
      val rhs = visitAst(right, lvalue = false, node).get
      val lhs = visitAst(left, lvalue = false, node).get
      val irOp = operator match {
        case AstPlus => Plus
        case AstMinus => Minus
        case AstTimes => Times
        case AstDivide => Divide
        case GreatThan => Gt
        case Equal => Eq
      }
      Some(builder.addInstruction(BinOp(irOp, lhs, rhs, SimpleType, loc), node))

    case Input(loc) =>
      Some(builder.addInstruction(LoadInput(loc), node))

    case OutputStmt(expr, loc) =>
      val value = visitAst(expr, lvalue = false, node).get
      builder.addInstruction(Print(value, loc), node)
      None

    case ReturnStmt(expr, loc) =>
      val value = visitAst(expr, lvalue = false, node).get
      builder.addInstruction(Return(value, loc), node)
      None

    case Identifier(name, loc) =>
      val variable = builder.getIdentifier(name)
      visitVariable(variable, lvalue, loc, node)

    case Number(value, loc) =>
      Some(builder.addInstruction(LoadImm(value, SimpleType, loc), node))

    case Null(loc) =>
      Some(builder.addInstruction(LoadImm(0, PointType(SimpleType), loc), node))

    case VarRef(id, loc) =>
      val variable = builder.getIdentifier(id.name)
      Some(builder.addInstruction(GetAddr(variable, loc), node))

    case Deref(expr, loc) =>
      val addr = visitAst(expr, lvalue, node).get
      Some(builder.addInstruction(Load(addr, loc), node))

    case CallFuncExpr(expr, args, loc) =>
      val fun = visitAst(expr, lvalue = false, node).get
      val compiledArgs = args.map(arg => visitAst(arg, lvalue = false, node).get)
      val tp = tAnalyzer.funRetIRType(expr)
      Some(builder.addInstruction(Call(fun, compiledArgs, tp, loc), node))

    case Record(fields, loc) =>
      val irFields = fields.map(f => visitAst(f.expr, lvalue = false, node).get)
      val size = irFields.foldLeft(0)((acc, f) => acc + f.tp.size)
      Some(builder.addInstruction(LoadComposed(irFields, ComposedType(size), loc), node))

    case FieldAccess(record, field, loc) =>
      val addrWithOffset = visitRecordAccess(record, recordLvalue = false, field, loc, node)
      visitVariable(addrWithOffset, lvalue, loc, node)

    case Array(elems, loc) =>
      val irElems = elems.map(e => visitAst(e, lvalue = false, node).get)
      val elSize = irElems.headOption.map(_.tp.size).getOrElse(0)
      Some(builder.addInstruction(LoadComposed(irElems, ComposedType(elSize * elems.size), loc), node))

    case ArrayAccess(array, index, loc) =>
      val addrWithOffset = visitArrayAccess(array, arrayLvalue = false, index, loc, node)
      visitVariable(addrWithOffset, lvalue, loc, node)

    case _ =>
      throw new RuntimeException(s"Unsupported ast node for compilation into x86IR: $ast")
  }

  /** Compiles a variable and loads it if the lvalue is set to false */
  private def visitVariable(addr: Instruction, lvalue: Boolean, loc: Loc, node: CfgStmtNode): Option[Instruction] = {
    if (lvalue) {
      Some(addr)
    } else {
      Some(builder.addInstruction(Load(addr, loc), node))
    }
  }

  /** Compiles a record access */
  private def visitRecordAccess(record: Expr, recordLvalue: Boolean, field: String, loc: Loc, node: CfgStmtNode): Instruction = {
    val addr = visitAst(record, recordLvalue, node).get
    val (tp, offset) = tAnalyzer.fieldInfo(record, field)
    val addrOffset = builder.addInstruction(LoadImm(-offset, PointType(tp), loc), node)
    builder.addInstruction(GetAddrOffset(addr, addrOffset, PointType(tp), loc), node)
  }

  /** Compiles an array access */
  private def visitArrayAccess(array: Expr, arrayLvalue: Boolean, index: Expr, loc: Loc, node: CfgStmtNode): Instruction = {
    val addr = visitAst(array, arrayLvalue, node).get
    val indexIr = visitAst(index, lvalue = false , node).get
    val tp = tAnalyzer.arrayElemTp(array)
    //multiply the index
    val multiplier = builder.addInstruction(LoadImm(-tp.size, PointType(tp), loc), node)
    val addrOffset = builder.addInstruction(BinOp(Times, indexIr, multiplier, PointType(tp), loc), node)
    builder.addInstruction(GetAddrOffset(addr, addrOffset, PointType(tp), loc), node)
  }
}
