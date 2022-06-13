package microc.backend.x86

import microc.backend.x86IR._
import microc.frontend.ast.{Identifier, IdentifierDecl, Loc}
import microc.middleend.cfg.CfgStmtNode
import utest._

object LiveInstructionAnalysisTest extends TestSuite {
  val proxyCfgNode1: CfgStmtNode = CfgStmtNode(Identifier("x", Loc(0, 0)))
  val proxyCfgNode2: CfgStmtNode = CfgStmtNode(Identifier("y", Loc(0, 0)))
  val proxyCfgNode3: CfgStmtNode = CfgStmtNode(Identifier("z", Loc(0, 0)))

  val tests: Tests = Tests {
    test("Live variable analysis test") {
      val program = createProgram
      val analysis = new LiveInstructionAnalysis(program)

      //live after LoadImm in bb1
      var actual = analysis.liveAfterInstr("bb1", LoadImm(5, SimpleType, Loc(0, 0)))
      actual ==> Set(LoadImm(5, SimpleType, Loc(0, 0)))

      //live after LoadInput in bb1
      actual = analysis.liveAfterInstr("bb1", LoadInput(Loc(0, 0)))
      actual ==> Set(LoadImm(5, SimpleType, Loc(0, 0)), LoadInput(Loc(0, 0)))

      //live after BinOp in bb1
      actual = analysis.liveAfterInstr("bb1", BinOp(Plus, LoadInput(Loc(0, 0)), LoadImm(5, SimpleType, Loc(0, 0)), SimpleType, Loc(0, 0)))
      Set(BinOp(Plus, LoadInput(Loc(0, 0)), LoadImm(5, SimpleType, Loc(0, 0)), SimpleType, Loc(0, 0)))
      actual ==> Set(BinOp(Plus, LoadInput(Loc(0, 0)), LoadImm(5, SimpleType, Loc(0, 0)), SimpleType, Loc(0, 0)))

      //live after CondJump in bb1
      actual = analysis.liveAfterInstr("bb1", CondJump(BinOp(Plus, LoadInput(Loc(0, 0)), LoadImm(5, SimpleType, Loc(0, 0)), SimpleType, Loc(0, 0)), "bb2", "bb3", Loc(0, 0)))
      actual ==> Set()

      //live after Load in bb2
      actual = analysis.liveAfterInstr("bb2", Load(Alloc(IdentifierDecl("x", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), Loc(0, 0)))
      actual ==> Set(
        Alloc(IdentifierDecl("y", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)),
        Load(Alloc(IdentifierDecl("x", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), Loc(0, 0)),
      )

      //live after Store in bb2
      actual = analysis.liveAfterInstr("bb2", Store(Alloc(IdentifierDecl("y", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), Load(Alloc(IdentifierDecl("x", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), Loc(0, 0)), Loc(0, 0)))
      actual ==> Set()
    }

    test("Order of instruction usage test") {
      val program = createProgram
      val analysis = new LiveInstructionAnalysis(program)

      //bb1
      analysis.usageOrder("bb1") ==> List(
        LoadInput(Loc(0, 0)),
        LoadImm(5, SimpleType, Loc(0, 0)),
        BinOp(Plus, LoadInput(Loc(0, 0)), LoadImm(5, SimpleType, Loc(0, 0)), SimpleType, Loc(0, 0)),
      )

      //bb2
      analysis.usageOrder("bb2") ==> List(
        Alloc(IdentifierDecl("x", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)),
        Alloc(IdentifierDecl("y", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)),
        Load(Alloc(IdentifierDecl("x", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), Loc(0, 0)),
      )

      //bb3
      analysis.usageOrder("bb3") ==> List(
        LoadImm(8, SimpleType, Loc(0, 0)),
        LoadComposed(List(LoadImm(8, SimpleType, Loc(0, 0))), ComposedType(8), Loc(0, 0)),
        LoadImm(10, SimpleType, Loc(0, 0)),
        GetAddrOffset(LoadComposed(List(LoadComposed(List(LoadImm(8, SimpleType, Loc(0, 0))), ComposedType(8), Loc(0, 0)), LoadImm(10, SimpleType, Loc(0, 0))), ComposedType(16), Loc(0, 0)), LoadImm(8, SimpleType, Loc(0, 0)), PointType(SimpleType), Loc(0, 0)),
        Load(GetAddrOffset(LoadComposed(List(LoadComposed(List(LoadImm(8, SimpleType, Loc(0, 0))), ComposedType(8), Loc(0, 0)), LoadImm(10, SimpleType, Loc(0, 0))), ComposedType(16), Loc(0, 0)), LoadImm(8, SimpleType, Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), Loc(0, 0)),
      )
    }
  }

  def createProgram: ProgramX86IR = {
    val bb1Instructions = List(
      Alloc(IdentifierDecl("x", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)),
      Alloc(IdentifierDecl("y", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)),
      Alloc(IdentifierDecl("z", Loc(0, 0)), PointType(ComposedType(16)), Loc(0, 0)),
      LoadImm(5, SimpleType, Loc(0, 0)),
      LoadInput(Loc(0, 0)),
      BinOp(Plus, LoadInput(Loc(0, 0)), LoadImm(5, SimpleType, Loc(0, 0)), SimpleType, Loc(0, 0)),
      CondJump(BinOp(Plus, LoadInput(Loc(0, 0)), LoadImm(5, SimpleType, Loc(0, 0)), SimpleType, Loc(0, 0)), "bb2", "bb3", Loc(0, 0))
    )

    val bb2Instructions = List(
      Load(Alloc(IdentifierDecl("x", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), Loc(0, 0)),
      Store(Alloc(IdentifierDecl("y", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), Load(Alloc(IdentifierDecl("x", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), Loc(0, 0)), Loc(0, 0)),
      Jump("bb3", Loc(0, 0))
    )

    val bb3Instructions = List(
      LoadImm(8, SimpleType, Loc(0, 0)),
      LoadComposed(List(LoadImm(8, SimpleType, Loc(0, 0))), ComposedType(8), Loc(0, 0)),
      LoadImm(10, SimpleType, Loc(0, 0)),
      LoadComposed(List(LoadComposed(List(LoadImm(8, SimpleType, Loc(0, 0))), ComposedType(8), Loc(0, 0)), LoadImm(10, SimpleType, Loc(0, 0))), ComposedType(16), Loc(0, 0)),
      Load(GetAddrOffset(LoadComposed(List(LoadComposed(List(LoadImm(8, SimpleType, Loc(0, 0))), ComposedType(8), Loc(0, 0)), LoadImm(10, SimpleType, Loc(0, 0))), ComposedType(16), Loc(0, 0)), LoadImm(8, SimpleType, Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), Loc(0, 0)),
      Return(Load(GetAddrOffset(LoadComposed(List(LoadComposed(List(LoadImm(8, SimpleType, Loc(0, 0))), ComposedType(8), Loc(0, 0)), LoadImm(10, SimpleType, Loc(0, 0))), ComposedType(16), Loc(0, 0)), LoadImm(8, SimpleType, Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), Loc(0, 0)), Loc(0, 0))
    )

    ProgramX86IR(List(
      Function(
        "main",
        0,
        List(
          BasicBlock("bb1", bb1Instructions, bb1Instructions.map(i => i -> proxyCfgNode1).toMap),
          BasicBlock("bb2", bb2Instructions, bb2Instructions.map(i => i -> proxyCfgNode2).toMap),
          BasicBlock("bb3", bb3Instructions, bb3Instructions.map(i => i -> proxyCfgNode3).toMap),
        )
      )
    ))
  }
}
