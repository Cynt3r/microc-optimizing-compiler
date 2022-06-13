package microc.middleend.optimization

import microc.frontend.ast._
import microc.middleend.Parser
import microc.middleend.analysis.{AnalysisHandlerInterface, Declarations, Types}
import microc.middleend.cfg.{BasicContext, BranchContext, CfgNodeContexts, CfgStmtNode, IfContext, ProgramCfg, WhileContext}
import utest._

import scala.collection.mutable

object OptimizerTest extends TestSuite with Parser {
  val tests: Tests = Tests {
    test("Test delete node optimization action") {
      val stmt = createStmt()
      val succ = stmt.succ.clone()
      val pred = stmt.pred.clone()
      val contexts: CfgNodeContexts = mutable.Map(stmt -> BasicContext)
      val optimizer = new Optimizer(proxyProgramCfg, DummyAnalysesInterface)
      optimizer.doOptimActions(List(DeleteNode(stmt)), contexts)

      //check if the node was really deleted
      succ.forall(s => !s.pred.contains(stmt)) ==> true
      pred.forall(p => !p.succ.contains(stmt)) ==> true

      //check if the node was deleted from contexts
      contexts.contains(stmt) ==> false

      //check if every toDelete predecessor was connected to every toDelete successor
      val connectOk = pred.forall(p => succ.forall(s => s.pred.contains(p) && p.succ.contains(s)))
      connectOk ==> true
    }

    test("Test replace node optimization action") {
      val stmt = createStmt()
      val entry = stmt.pred.head
      val search = Identifier("x", Loc(0, 0))
      val replace = Number(5, Loc(0, 0))
      val contexts: CfgNodeContexts = mutable.Map(
        entry -> IfContext(
          Some(BranchContext(entry, Set(stmt))),
          Some(BranchContext(stmt, Set(entry)))
        ),
        stmt -> BasicContext
      )
      val optimizer = new Optimizer(proxyProgramCfg, DummyAnalysesInterface)
      optimizer.doOptimActions(List(ReplaceNode(search, replace, stmt)), contexts)

      val replacedCfgNode = entry.succ.head
      replacedCfgNode.ast ==> AssignStmt(Identifier("y", Loc(0, 0)), BinaryOp(Plus, replace, replace, Loc(0, 0)), Loc(0, 0))

      //check if contexts was properly updated with a new node
      contexts(replacedCfgNode) ==> BasicContext
      contexts(entry) ==> IfContext(
        Some(BranchContext(entry, Set(replacedCfgNode))),
        Some(BranchContext(replacedCfgNode, Set(entry)))
      )
    }

    test("Test connect nodes optimization action") {
      val src = CfgStmtNode(Null(Loc(0, 0)))
      val dest = CfgStmtNode(Null(Loc(0, 0)))
      val contexts: CfgNodeContexts = mutable.Map()
      val optimizer = new Optimizer(proxyProgramCfg, DummyAnalysesInterface)
      optimizer.doOptimActions(List(ConnectNodes(src, dest)), contexts)

      //check if nodes are really connected to each other
      src.succ ==> Set(dest)
      src.pred ==> Set()
      dest.pred ==> Set(src)
      dest.succ ==> Set()
    }

    test("Test disconnect node optimization action without keeping contexts") {
      val stmt = createStmt()
      val succ = stmt.succ.clone()
      val pred = stmt.pred.clone()
      val contexts: CfgNodeContexts = mutable.Map(stmt -> BasicContext)
      val optimizer = new Optimizer(proxyProgramCfg, DummyAnalysesInterface)
      optimizer.doOptimActions(List(DisconnectNode(stmt, keepContexts = false)), contexts)

      //check if node was really deleted
      succ.forall(s => !s.pred.contains(stmt) && !stmt.succ.contains(s)) ==> true
      pred.forall(p => !p.succ.contains(stmt) && !stmt.pred.contains(p)) ==> true

      //check if the node was deleted from contexts
      contexts.contains(stmt) ==> false
    }

    test("Test disconnect node optimization action with keeping contexts") {
      val stmt = createStmt()
      val succ = stmt.succ.clone()
      val pred = stmt.pred.clone()
      val contexts: CfgNodeContexts = mutable.Map(stmt -> BasicContext)
      val optimizer = new Optimizer(proxyProgramCfg, DummyAnalysesInterface)
      optimizer.doOptimActions(List(DisconnectNode(stmt, keepContexts = true)), contexts)

      //check if node was really deleted
      succ.forall(s => !s.pred.contains(stmt) && !stmt.succ.contains(s)) ==> true
      pred.forall(p => !p.succ.contains(stmt) && !stmt.pred.contains(p)) ==> true

      //node context should still exist
      contexts.contains(stmt) ==> true
    }

    test("Test add declaration optimization action") {
      val succ = CfgStmtNode(Null(Loc(0, 0)))
      val stmt = CfgStmtNode(VarStmt(
        List(
          IdentifierDecl("x", Loc(0, 1)),
          IdentifierDecl("y", Loc(0, 2)),
        ),
        Loc(0, 0)
      ))
      succ.pred.add(stmt)
      stmt.succ.add(succ)
      val contexts: CfgNodeContexts = mutable.Map(stmt -> BasicContext)
      val optimizer = new Optimizer(proxyProgramCfg, DummyAnalysesInterface)
      val action = AddDeclaration(IdentifierDecl("z", Loc(0, 3)), IntType, List(), stmt)
      optimizer.doOptimActions(List(action), contexts)

      //check if connections were preserved
      succ.pred.nonEmpty ==> true
      val newStmt = succ.pred.head
      newStmt.succ ==> Set(succ)

      //check if contexts were properly updated
      contexts.contains(stmt) ==> false
      contexts(newStmt) ==> BasicContext

      //check if declaration was added
      newStmt.ast.asInstanceOf[VarStmt].decls ==> List(
        IdentifierDecl("x", Loc(0, 1)),
        IdentifierDecl("y", Loc(0, 2)),
        IdentifierDecl("z", Loc(0, 3)),
      )
    }

    test("Test delete declaration optimization action") {
      val succ = CfgStmtNode(Null(Loc(0, 0)))
      val stmt = CfgStmtNode(VarStmt(
        List(
          IdentifierDecl("x", Loc(0, 1)),
          IdentifierDecl("y", Loc(0, 2)),
        ),
        Loc(0, 0)
      ))
      succ.pred.add(stmt)
      stmt.succ.add(succ)
      val contexts: CfgNodeContexts = mutable.Map(stmt -> BasicContext)
      val optimizer = new Optimizer(proxyProgramCfg, DummyAnalysesInterface)
      val action = DeleteDeclaration(IdentifierDecl("x", Loc(0, 1)), stmt)
      optimizer.doOptimActions(List(action), contexts)

      //check if connections were preserved
      succ.pred.nonEmpty ==> true
      val newStmt = succ.pred.head
      newStmt.succ ==> Set(succ)

      //check if contexts were properly updated
      contexts.contains(stmt) ==> false
      contexts(newStmt) ==> BasicContext

      //check if declaration was removed
      newStmt.ast.asInstanceOf[VarStmt].decls ==> List(IdentifierDecl("y", Loc(0, 2)))
    }

    test("Test prepend node optimization action") {
      val stmt = createStmt()
      val succ = stmt.succ.clone()
      val pred = stmt.pred.clone()
      val contexts: CfgNodeContexts = mutable.Map(stmt -> BasicContext)
      val optimizer = new Optimizer(proxyProgramCfg, DummyAnalysesInterface)
      val newStmt = CfgStmtNode(Null(Loc(0, 42)))
      optimizer.doOptimActions(List(PrependNode(newStmt, BasicContext, stmt)), contexts)

      //pred>--<newStmt
      pred.forall(_.succ == Set(newStmt)) ==> true
      newStmt.pred ==> pred

      //newStmt>--<stmt
      newStmt.succ ==> Set(stmt)
      stmt.pred ==> Set(newStmt)

      //stmt>--<succ
      stmt.succ ==> succ
      succ.forall(_.pred == Set(stmt)) ==> true

      //check if context was added
      contexts.contains(newStmt)
    }

    test("Test change context optimization action") {
      val stmt = createStmt()
      val contexts: CfgNodeContexts = mutable.Map(stmt -> BasicContext)
      val newContext = WhileContext(None)
      val optimizer = new Optimizer(proxyProgramCfg, DummyAnalysesInterface)
      val action = ChangeContext(newContext, stmt)
      optimizer.doOptimActions(List(action), contexts)

      contexts(stmt) ==> newContext
    }

    /*
    test("Test the whole optimizer with optimizations on") {
      val cfg = testCfg
      val origVarStmt = cfg.entryNodes.head.succ.head
      val optimizer = new Optimizer(cfg, DummyAnalysesInterface, 500)
      optimizer.run()
      val newVarStmt = cfg.entryNodes.head.succ.head

      assert(origVarStmt != newVarStmt)
    }


    test("Test the whole optimizer with optimizations off") {
      val cfg = testCfg
      val origVarStmt = cfg.entryNodes.head.succ.head
      val optimizer = new Optimizer(cfg, DummyAnalysesInterface, 0)
      optimizer.run()
      val newVarStmt = cfg.entryNodes.head.succ.head

      origVarStmt ==> newVarStmt
    }
    */
  }

  private def proxyProgramCfg = new ProgramCfg(Program(List(), Loc(0, 0)), Set())(mutable.Map())

  //sets up CfgStmtNode with some successors and predecessors for testing
  private def createStmt(): CfgStmtNode = {
    val pred1 = CfgStmtNode(Null(Loc(0, 0)))
    val pred2 = CfgStmtNode(Null(Loc(0, 1)))
    val succ1 = CfgStmtNode(Null(Loc(1, 0)))
    val succ2 = CfgStmtNode(Null(Loc(1, 1)))
    val stmt = CfgStmtNode(AssignStmt(
      Identifier("y", Loc(0, 0)),
      BinaryOp(Plus, Identifier("x", Loc(0, 0)), Identifier("x", Loc(0, 0)), Loc(0, 0)),
      Loc(0, 0))
    )

    pred1.succ.add(stmt)
    pred2.succ.add(stmt)
    stmt.pred.add(pred1)
    stmt.pred.add(pred2)
    stmt.succ.add(succ1)
    stmt.succ.add(succ2)
    succ1.pred.add(stmt)
    succ2.pred.add(stmt)
    stmt
  }

  /*
  private def testCfg: ProgramCfg = {
    val code =
      """
        |main() {
        |  var x, y, z;
        |  return 0;
        |}""".stripMargin
    parseCode(code)._1
  }
  */

  object DummyAnalysesInterface extends AnalysisHandlerInterface {
    def getTypes(program: Program): Types = Map()

    /** Returns result of a declaration analysis */
    def getDeclarations(program: Program): Declarations = Map()
  }
}
