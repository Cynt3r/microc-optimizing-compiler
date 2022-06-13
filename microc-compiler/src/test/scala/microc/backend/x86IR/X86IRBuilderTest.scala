package microc.backend.x86IR

import microc.frontend.ast.{FunBlockStmt, FunDecl, IdentifierDecl, Loc, Null, ReturnStmt}
import microc.middleend.cfg.CfgStmtNode
import utest._

object X86IRBuilderTest extends TestSuite {
  private val proxyCfgNode: CfgStmtNode = CfgStmtNode(Null(Loc(0, 0)))

  val tests: Tests = Tests {
    test("Basic x86IR builder test") {
      val b = new X86IRBuilder()
      b.enterFunction(mainFun)
      val x = b.addInstruction(Alloc(IdentifierDecl("x", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), proxyCfgNode)
      b.addVariable(x.asInstanceOf[Alloc])
      val y = b.addInstruction(Alloc(IdentifierDecl("y", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), proxyCfgNode)
      b.addVariable(y.asInstanceOf[Alloc])
      b.addInstruction(Jump("foo", Loc(0, 0)), proxyCfgNode)
      val bb = b.createBbNewSuffix()
      b.enterBb(bb)
      b.addInstruction(Return(y, Loc(0, 0)), proxyCfgNode)
      b.leaveFunction()
      b.enterFunction(fooFun)
      val z = b.addInstruction(Alloc(IdentifierDecl("z", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)), proxyCfgNode)
      b.addVariable(z.asInstanceOf[Alloc])
      b.addInstruction(Jump("foo", Loc(0, 0)), proxyCfgNode)
      b.leaveFunction()
      val program = b.build()

      //function count check
      program.functions.size ==> 2
      val main = program.functions.head
      main.name ==> "main"
      val foo = program.functions.tail.head
      foo.name ==> "foo"

      //basic block contains check
      main.bbs.size ==> 2
      foo.bbs.size ==> 1

      //checks for main's first basic block
      val mainBb1 = main.bbs.head
      mainBb1.name ==> "bb_1"
      mainBb1.instructions ==> List(
        Alloc(IdentifierDecl("x", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)),
        Alloc(IdentifierDecl("y", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)),
        Jump("foo", Loc(0, 0)),
      )
      mainBb1.sourceCfgNodes ==> Map(
        Alloc(IdentifierDecl("x", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)) -> proxyCfgNode,
        Alloc(IdentifierDecl("y", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)) -> proxyCfgNode,
        Jump("foo", Loc(0, 0)) -> proxyCfgNode,
      )

      //name check of second basic block
      val mainBb2 = main.bbs.tail.head
      mainBb2.name ==> "bb_2"
    }

    test("Adding instruction with no active function") {
      intercept[RuntimeException] {
        new X86IRBuilder().addInstruction(LoadInput(Loc(0, 0)), proxyCfgNode)
      }
    }

    test("Leaving function with no active function") {
      intercept[RuntimeException] {
        new X86IRBuilder().leaveFunction()
      }
    }

    test("Adding variable with no active function") {
      intercept[RuntimeException] {
        new X86IRBuilder().addVariable(Alloc(IdentifierDecl("x", Loc(0, 0)), PointType(SimpleType), Loc(0, 0)))
      }
    }

    test("Getting variable with no active function") {
      intercept[RuntimeException] {
        new X86IRBuilder().getIdentifier("x")
      }
    }

    test("Entering basic block with no active function") {
      val b = new X86IRBuilder()
      val bb = b.createBbNewSuffix()
      intercept[RuntimeException] {
        b.enterBb(bb)
      }
    }

    test("Correct basic block name generation") {
      val b = new X86IRBuilder()
      val bb1 = b.createBbNewSuffix()
      val bbIf = b.createBbNewSuffix("bb_if")
      val bbElse = b.createBbSameSuffix("bb_else")
      val bb2 = b.createBbNewSuffix()

      bb1.name ==> "bb_1"
      bbIf.name ==> "bb_if_2"
      bbElse.name ==> "bb_else_2"
      bb2.name ==> "bb_3"
    }

    test("Correct basic block name collision handling") {
      val b = new X86IRBuilder()
      val bb1 = b.createBbNewSuffix("else")
      val bb2 = b.createBbSameSuffix("else")
      val bb3 = b.createBbSameSuffix("else")

      bb1.name ==> "else_1"
      bb2.name ==> "else_2"
      bb3.name ==> "else_3"
    }

    test("Correct function name collision handling") {
      val b = new X86IRBuilder()
      val fun = FunAlloc(FunDecl("main_1", List(), funBlock, Loc(0, 0)), Loc(0, 0))
      val bb = b.createBbNewSuffix("main")
      b.enterFunction(fun)

      bb.name ==> "main_1"
      b.getIdentifier("main_1_2") ==> fun
    }

    test("Adding instruction to terminated basic block") {
      val b = new X86IRBuilder()
      b.enterFunction(mainFun)
      b.addInstruction(Jump("bb_2", Loc(0, 0)), proxyCfgNode)
      intercept[RuntimeException] {
        b.addInstruction(LoadInput(Loc(0, 0)), proxyCfgNode)
      }
    }

    test("Building non-terminated basic block") {
      val b = new X86IRBuilder()
      b.enterFunction(mainFun)
      intercept[RuntimeException] {
        //leaving a function triggers build of current active basic block
        b.leaveFunction()
      }
    }
  }

  def funBlock: FunBlockStmt = FunBlockStmt(List(), List(), ReturnStmt(Null(Loc(0, 0)), Loc(0, 0)), Loc(0, 0))

  def mainFun: FunAlloc = FunAlloc(FunDecl("main", List(), funBlock, Loc(0, 0)), Loc(0, 0))

  def fooFun: FunAlloc = FunAlloc(FunDecl("foo", List(), funBlock, Loc(0, 0)), Loc(0, 0))
}
