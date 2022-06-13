package microc.backend.x86

import microc.backend.x86.helpers._
import microc.backend.x86IR._
import microc.frontend.ast.{IdentifierDecl, Loc}
import utest._

object CompilerMemoryTest extends TestSuite {
  val tests: Tests = Tests {
    test("Stack operations (alloc frame, load, spill back") {
      val builder = new X86Builder()
      val memory = new CompilerMemory(analysis, builder)
      val arg1 = Alloc(IdentifierDecl("x", proxyLoc), PointType(SimpleType), proxyLoc)
      val arg2 = Alloc(IdentifierDecl("y", proxyLoc), PointType(SimpleType), proxyLoc)
      val local1 = Alloc(IdentifierDecl("a", proxyLoc), PointType(SimpleType), proxyLoc)
      val local2 = Alloc(IdentifierDecl("b", proxyLoc), PointType(SimpleType), proxyLoc)
      val local3 = Alloc(IdentifierDecl("c", proxyLoc), PointType(SimpleType), proxyLoc)
      val load = Load(Alloc(IdentifierDecl("c", proxyLoc), PointType(ComposedType(24)), proxyLoc), proxyLoc)
      memory.allocFrame(16, 24)

      memory.allocateTempOnStack(load)
      memory.addArg(arg1)
      memory.addArg(arg2)
      memory.addLocal(local1)
      memory.addLocal(local2)
      memory.addLocal(local3)
      //previous calls should not generate any x86 instructions
      builder.getInstructions.size ==> 0

      //offset checks
      memory.findMemLoc(arg1, Set(), "bb") ==> (RBP(X86RegMode.M64), 24)
      memory.findMemLoc(arg2, Set(), "bb") ==> (RBP(X86RegMode.M64), 16)
      memory.findMemLoc(local1, Set(), "bb") ==> (RBP(X86RegMode.M64), -8)
      memory.findMemLoc(local2, Set(), "bb") ==> (RBP(X86RegMode.M64), -16)
      memory.findMemLoc(local3, Set(), "bb") ==> (RBP(X86RegMode.M64), -24)
      memory.findMemLoc(load, Set(), "bb")._2 ==> 0

      //clear frame and check if everything got cleared correctly
      memory.clearFrame() ==> 24
      intercept[Exception] {memory.findMemLoc(load, Set(), "bb")}
      intercept[Exception] {memory.findMemLoc(arg1, Set(), "bb")}
      intercept[Exception] {memory.findMemLoc(arg2, Set(), "bb")}
      intercept[Exception] {memory.findMemLoc(local1, Set(), "bb")}
      intercept[Exception] {memory.findMemLoc(local2, Set(), "bb")}
      intercept[Exception] {memory.findMemLoc(local3, Set(), "bb")}
    }

    test("Register operations") {
      val builder = new X86Builder()
      val memory = new CompilerMemory(analysis, builder)
      memory.allocFrame(0, 0)

      //alloc
      val reg = memory.allocReg(LoadImm(42, SimpleType, proxyLoc), Set(), "bb")
      builder.getInstructions.size ==> 0

      //realloc
      memory.reallocReg(reg.id, Set(), "bb")
      builder.getInstructions.size ==> 1
      assertMatch(builder.getInstructions.head){case MovRegReg(_, r, _) if r == reg =>}
      val newReg = builder.getInstructions.head.asInstanceOf[MovRegReg].dest

      //load
      memory.load(LoadImm(42, SimpleType, proxyLoc), Set(), "bb") ==> newReg
      builder.getInstructions.size ==> 1
    }

    test("Scratch register persistence") {
      val builder = new X86Builder()
      val memory = new CompilerMemory(analysis, builder)
      memory.allocFrame(0, 0)

      memory.assignToReg(LoadImm(42, SimpleType, proxyLoc), RAX(X86RegMode.M32))
      memory.assignToReg(LoadImm(69, SimpleType, proxyLoc), R10(X86RegMode.M32))
      builder.getInstructions.size ==> 0

      memory.saveScratchRegs(List())
      builder.getInstructions.size ==> 2
      builder.getInstructions.contains(PushReg(RAX(X86RegMode.M64)))
      builder.getInstructions.contains(PushReg(R10(X86RegMode.M64)))
      //determine which register got pushed first and which second
      val (first, second) = builder.getInstructions.head match {
        case PushReg(RAX(X86RegMode.M64), _) => (RAX(X86RegMode.M64), R10(X86RegMode.M64))
        case _ => (R10(X86RegMode.M64), RAX(X86RegMode.M64))
      }

      memory.loadRegs(List(first.id, second.id))
      val expected = List(
        PushReg(first),
        PushReg(second),
        PopReg(second),
        PopReg(first),
      )
      builder.getInstructions ==> expected
    }

    test("Correct register mode usage") {
      val loadInt = LoadImm(0, SimpleType, proxyLoc)
      val loadAddr = LoadImm(0, PointType(SimpleType), proxyLoc)
      val builder = new X86Builder()
      val memory = new CompilerMemory(analysis, builder)
      memory.allocFrame(0, 0)

      //allocReg
      memory.allocReg(loadInt, Set(), "bb").mode ==> X86RegMode.M32
      memory.allocReg(loadAddr, Set(), "bb").mode ==> X86RegMode.M64

      //load
      memory.load(loadInt, Set(), "bb").mode ==> X86RegMode.M32
      memory.load(loadAddr, Set(), "bb").mode ==> X86RegMode.M64

      memory.deallocAllRegs()

      //load from stack
      memory.allocateTempOnStack(loadAddr)
      memory.load(loadAddr, Set(), "bb").mode ==> X86RegMode.M64
      memory.allocateTempOnStack(loadInt)
      memory.load(loadInt, Set(), "bb").mode ==> X86RegMode.M32

      memory.clearFrame() ==> 8 //size of an int in stack memory
    }

    test("Correct spilling priority") {
      val instructions = (0 to 15).map(i => Print(LoadImm(i, SimpleType, proxyLoc), proxyLoc)).toList
      val lia = new LiveInstructionAnalysis(ProgramX86IR(List(
        Function(
          "main",
          0,
          List(BasicBlock("bb", instructions, Map())),
        )
      )))
      val builder = new X86Builder()
      val memory = new CompilerMemory(lia, builder)
      memory.allocFrame(0, 0)

      //assign each instruction to a register (this will fill all of the available registers)
      instructions.zipWithIndex.foreach(x => memory.assignToReg(x._1.target, Register.apply(x._2, X86RegMode.M32)))


      //allocating instruction will cause spilling of the last LoadImm with index 15
      val reg = memory.load(LoadImm(15, SimpleType, proxyLoc), Set(), "bb")
      memory.allocReg(LoadImm(16, SimpleType, proxyLoc), Set(), "bb") ==> reg

      //spill move check
      builder.getInstructions ==> List(MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), reg))

      //check size of the spilled temporary value
      memory.clearFrame() ==> 8
    }
  }

  private def proxyLoc = Loc(0, 0)

  private def analysis = new LiveInstructionAnalysis(ProgramX86IR(List()))
}
