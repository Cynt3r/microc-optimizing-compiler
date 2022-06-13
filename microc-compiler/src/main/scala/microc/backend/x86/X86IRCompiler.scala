package microc.backend.x86

import microc.X86Syntax.X86Syntax
import microc.backend.x86.helpers._
import microc.backend.x86IR.{Alloc, ArgAddr, BasicBlock, BinOp, ComposedType, CondJump, Divide, Eq, Function, GetAddr, GetAddrOffset, Gt, HeapAlloc, Jump, Load, LoadComposed, LoadImm, LoadInput, Minus, Plus, PointType, Print, ProgramX86IR, Return, Store, Times, Call => IRCall, Instruction => IRInstruction}

/**
 * Compiler that takes x86 IR and produces x86 assembly
 * @param program x86IR of the program
 * @param syntax syntax of the output assembly
 * @param optimize if true, compiler will perform peephole optimizations
 */
class X86IRCompiler(program: ProgramX86IR, syntax: X86Syntax, optimize: Boolean) {
  private val builder = new X86Builder()
  private val lia = new LiveInstructionAnalysis(program)
  private val memory = new CompilerMemory(lia, builder)
  private var labelCnt = 0

  /** Compiles x86 IR into x86 assembly string */
  def compile(): String = {
    compileProgram()
    //build x86 assembly
    builder.build(syntax, optimize)
  }

  /** Compiles x86 IR into list of x86 instructions */
  def compileToInstructions(): List[Instruction] = {
    compileProgram()
    builder.getInstructions
  }

  private def compileProgram(): Unit = {
    builder.add(Label("_start"))
    //assign each function value to its respective memory location
    program.functions.foreach(f => {
      val memLocation = "__" + f.name
      builder.registerFun(f.name, "__" + f.name)
      builder.add(MovRegLabel(RAX(X86RegMode.M64), LabelOp(f.name)))
      builder.add(MovMemReg(Mem(LabelOp(memLocation)), RAX(X86RegMode.M64)))
    })
    //call main and then exit
    val mainName = program.mainFunction.name
    builder.add(CallLabel(LabelOp(mainName)))
    builder.add(CallLabel(LabelOp("exit")))
    //compile each function
    program.functions.foreach(compileFunction)
  }

  private def compileFunction(fun: Function): Unit = {
    builder.add(Label(fun.name))
    //save previous base pointer and set it to current stack pointer
    builder.add(PushReg(RBP(X86RegMode.M64), Some("Save previous base pointer")))
    builder.add(MovRegReg(RBP(X86RegMode.M64), RSP(X86RegMode.M64), Some("Update base pointer")))

    //calculate bytesize of variables that will be stored in a stack frame
    val argsSize = fun.getParams.map(_.tp.pointsTo.size).sum
    val localsSize = fun.getLocals.map(_.tp.pointsTo.size).sum
    memory.allocFrame(argsSize, localsSize)

    //this instruction will be patched later with correct stack size
    val toPatch = builder.add(SubRegImm(RSP(X86RegMode.M64), Imm(0)))

    //compile each basic block (stack size = frame size + return address + base pointer)
    fun.bbs.foreach(compileBb)

    //patch correct size of a stack frame
    val tempsSize = memory.clearFrame()
    val frameSize = localsSize + tempsSize
    val alignment = frameSize % 16
    builder.patch(toPatch, SubRegImm(RSP(X86RegMode.M64), Imm(frameSize + alignment), Some("Allocate frame space on stack")))
  }

  private def compileBb(bb: BasicBlock): Unit = {
    builder.add(Label(bb.name))
    bb.instructions.foreach(i => {
      //compile only instructions that either have side-effects or are live after their execution
      if (!isUseless(i, bb.name)) {
        compileInstruction(i, bb.name)
      }
      memory.cleanDeadInstructions(bb.name, i)
    })
  }

  private def compileInstruction(instr: IRInstruction, bbName: String): Unit = instr match {
    case alloc: Alloc =>
      memory.addLocal(alloc)

    case ArgAddr(alloc, _) =>
      memory.addArg(alloc)

    case HeapAlloc(value, _) =>
      //realloc RAX which will contain memory address of the allocation
      memory.reallocReg(RAX(X86RegMode.M64).id, Set(value), bbName)
      //persist caller saved registers (except RAX)
      val savedRegs = memory.saveScratchRegs(List(RAX(X86RegMode.M64)))
      //align stack to 16 bytes if needed
      val aligned = alignStack(savedRegs.length * 8)
      //push number of bytes to allocate to RDI (first and only argument of a malloc)
      builder.add(MovRegImm(RDI(X86RegMode.M64), Imm(value.tp.size)))
      builder.add(CallLabel(LabelOp("malloc"), Some("Call malloc()")))
      dealignStack(aligned)
      //reload caller saved registers
      memory.loadRegs(savedRegs)
      //result is in RAX
      memory.assignToReg(instr, RAX(X86RegMode.M64))
      //load init value and copy it into the newly allocated memory location
      val reg = memory.load(value, Set(), bbName)
      value.tp match {
        case ComposedType(size) =>
          val workRegId = memory.createEmptyReg(Set(value, instr), bbName)
          val workReg = Register(workRegId, reg.mode)
          copyRecord(workReg, reg, RAX(X86RegMode.M64), 0, size)

        case _ =>
          builder.add(MovMemReg(getMem(RAX(X86RegMode.M64), 0), reg))
      }

    case Store(dest, src, _) =>
      val (addrReg, addrOffset) = memory.findMemLoc(dest, Set(src), bbName)
      val srcReg = memory.load(src, Set(dest), bbName)
      src.tp match {
        //composed types are loaded into registers and then copied back on stack
        case ComposedType(size) =>
          //prepare empty register
          val workRegId = memory.createEmptyReg(Set(src), bbName)
          val workReg = Register(workRegId, srcReg.mode)
          //copy record into correct part of a stack designated for dest
          copyRecord(workReg, srcReg, addrReg, addrOffset, size)

        //every other type is copied from register
        case _ =>
          builder.add(MovMemReg(getMem(addrReg, addrOffset), srcReg))
      }

    case LoadImm(value, _, _) =>
      val reg = memory.allocReg(instr, Set(), bbName)
      builder.add(MovRegImm(reg, Imm(value)))

    case Load(src, _) =>
      val destReg = memory.allocReg(instr, Set(src), bbName)
      val (addrReg, addrOffset) = memory.findMemLoc(src, Set(instr), bbName)
      src.tp match {
        //in case of Composed type load the address
        case PointType(ComposedType(_)) =>
          memAddrToReg(destReg, addrReg, addrOffset)

        //in case of other types just load the value from stack
        case PointType(_) =>
          builder.add(MovRegMem(destReg, getMem(addrReg, addrOffset)))

        case _ =>
          throw new RuntimeException("Internal error: can't load non-pointer type")
      }

    case LoadComposed(fields, _, _) =>
      //alloc register for an address of the record
      val reg = memory.allocReg(instr, Set(), bbName)
      //alloc space on stack for the record
      val offset = memory.allocateTempOnStack(instr)
      //copy fields into allocated space on the stack
      fields.foldLeft(0)((currOffset, field) => {
        val fieldOffset = offset - currOffset

        field.tp match {
          //address of a composed type is loaded into a register and then each 8 bytes are copied from that address to registers and then back to stack
          case ComposedType(size) =>
            val fieldAddr = memory.load(field, Set(instr), bbName)
            //copy sub-record
            copyRecord(reg, fieldAddr, RBP(X86RegMode.M64), fieldOffset, size)

          //other types are loaded into a register and then copied into the memory
          case _ =>
            val fieldReg = memory.load(field, Set(instr), bbName)
            builder.add(MovMemReg(MemOffset(RBP(X86RegMode.M64), fieldOffset), fieldReg))
        }
        currOffset + field.tp.size
      })

      //save address into a register
      builder.add(MovRegReg(reg, RBP(X86RegMode.M64)))
      builder.add(AddRegImm(reg, Imm(offset)))

    case GetAddr(alloc, _) =>
      val reg = memory.allocReg(instr, Set(), bbName)
      val (addrReg, addrOffset) = memory.findMemLoc(alloc, Set(instr), bbName)
      memAddrToReg(reg, addrReg, addrOffset)

    case GetAddrOffset(addr, offset, _, _) =>
      val reg = memory.allocReg(instr, Set(addr, offset), bbName)
      val (addrReg, addrOffset) = memory.findMemLoc(addr, Set(instr), bbName)
      memAddrToReg(reg, addrReg, addrOffset)
      //add the offset
      val regOffset = memory.load(offset, Set(instr, addr), bbName)
      builder.add(AddRegReg(reg, regOffset))

    case BinOp(op, lhs, rhs, tp, _) =>
      val lhsRegId = memory.load(lhs, Set(rhs), bbName).id
      //register mode depends on IRType of BinOp
      val lhsReg = Register(lhsRegId, memory.getRegMode(tp))
      val rhsRegId = memory.load(rhs, Set(lhs), bbName).id
      //again, register mode depends on IRType of BinOp
      val rhsReg = Register(rhsRegId, memory.getRegMode(tp))

      op match {
        case Plus | Minus | Times =>
          val resReg = memory.allocReg(instr, Set(lhs, rhs), bbName)
          builder.add(MovRegReg(resReg, lhsReg))
          op match {
            case Plus => builder.add(AddRegReg(resReg, rhsReg))
            case Minus => builder.add(SubRegReg(resReg, rhsReg))
            case Times => builder.add(MulRegReg(resReg, rhsReg))
            case _ => throw new RuntimeException("This exception will never happen")
          }

        case Divide =>
          //reallocate content of RAX, since it will be overwritten
          memory.reallocReg(RAX(X86RegMode.M64).id, Set(lhs, rhs), bbName)
          //since rhs could've been moved from RAX, we need to get its (possibly) new register
          val reloadRhsReg = memory.load(rhs, Set(lhs), bbName)
          //push RDX to the stack so its value doesn't get overwritten
          builder.add(PushReg(RDX(X86RegMode.M64)))
          //clear high bits of dividend
          builder.add(MovRegImm(RDX(X86RegMode.M32), Imm(0)))
          builder.add(MovRegReg(RAX(X86RegMode.M32), lhsReg))
          builder.add(DivReg(reloadRhsReg))
          //pop RDX back
          builder.add(PopReg(RDX(X86RegMode.M64)))
          //result is in RAX
          memory.assignToReg(instr, RAX(X86RegMode.M32))

        case Eq | Gt =>
          val resReg = memory.allocReg(instr, Set(lhs, rhs), bbName)
          builder.add(CmpRegReg(lhsReg, rhsReg))
          val (falseLbl, afterLbl) = op match {
            case Eq =>
              val notEqLbl = nextLabel(bbName + "_neq")
              val afterEqLbl = sameLabel(bbName + "_afterEq")
              builder.add(Jne(LabelOp(notEqLbl))) //if lhs != rhs, jump to notEqLbl
              (notEqLbl, afterEqLbl)

            case Gt =>
              val leLbl = nextLabel(bbName + "_leq")
              val afterGtLbl = sameLabel(bbName + "_afterGt")
              builder.add(Jle(LabelOp(leLbl))) //if lhs <= rhs, jump to leLbl
              (leLbl, afterGtLbl)

            case _ =>
              throw new RuntimeException("This exception will never happen")
          }
          builder.add(MovRegImm(resReg, Imm(1)))
          builder.add(Jmp(LabelOp(afterLbl))) // cmp resulted in true, jump to afterGtLbl
          builder.add(Label(falseLbl))
          builder.add(MovRegImm(resReg, Imm(0)))
          builder.add(Label(afterLbl))
      }

    case _: LoadInput =>
      val reg = memory.allocReg(instr, Set(), bbName)
      //persist caller saved registers (except reg)
      val savedRegs = memory.saveScratchRegs(List(reg))
      //align stack to 16 bytes if needed
      val aligned = alignStack(savedRegs.length * 8)
      builder.add(MovRegLabel(RSI(X86RegMode.M64), LabelOp("scanfRes"), Some("Address of scanned integer")))
      builder.add(MovRegLabel(RDI(X86RegMode.M64), LabelOp("scanfFmt"), Some("Format string for scanf()")))
      builder.add(CallLabel(LabelOp("scanf"), Some("Call scanf()")))
      dealignStack(aligned)

      val notEofLbl = nextLabel(bbName + "_notEof")
      val afterLbl = sameLabel(bbName + "_afterInput")
      //compare the return value of scanf
      builder.add(CmpRegImm(RAX(X86RegMode.M32), Imm(1)))
      builder.add(Je(LabelOp(notEofLbl)))
      //if the return value is not equal to 1 (number of successfully scanned items), EOF was detected -> load -1 (microC semantic of the Input)
      builder.add(MovRegImm(reg, Imm(-1)))
      builder.add(Jmp(LabelOp(afterLbl)))
      //otherwise load the scanned value
      builder.add(Label(notEofLbl))
      builder.add(MovRegMem(reg, Mem(LabelOp("scanfRes"))))

      builder.add(Label(afterLbl))
      //reload caller saved registers (without the reg, which already contains correct value)
      memory.loadRegs(savedRegs)

    case Print(target, _) =>
      val reg = memory.load(target, Set(), bbName)
      //persist caller saved registers (except reg)
      val savedRegs = memory.saveScratchRegs(List(reg))
      //align stack to 16 bytes if needed
      val aligned = alignStack(savedRegs.length * 8)
      builder.add(MovRegReg(RSI(X86RegMode.M32), reg, Some("Value to be printed")))
      builder.add(MovsxRegReg(RSI(X86RegMode.M64), RSI(X86RegMode.M32), Some("Sign extension")))
      builder.add(MovRegLabel(RDI(X86RegMode.M64), LabelOp("printfFmt"), Some("Format string for printf()")))
      builder.add(CallLabel(LabelOp("printf"), Some("Call printf()")))
      dealignStack(aligned)
      //reload caller saved registers
      memory.loadRegs(savedRegs)

    case Return(value, _) =>
      val reg = memory.load(value, Set(), bbName)
      builder.add(MovRegReg(RAX(reg.mode), reg))
      //restore stack pointer
      builder.add(MovRegReg(RSP(X86RegMode.M64), RBP(X86RegMode.M64), Some("Reset stack pointer")))
      //restore base pointer from stack
      builder.add(PopReg(RBP(X86RegMode.M64), Some("Restore base pointer")))
      builder.add(Ret())
      //since function ends with return, we can clear all registers with no consequences
      memory.deallocAllRegs()

    case Jump(target, _) =>
      //since jump is last instruction of basic block, save all variables to stack and clear registers
      memory.cleanDeadInstructions(bbName, instr)
      memory.deallocAllRegs()
      builder.add(Jmp(LabelOp(target)))

    case CondJump(cond, trueTarget, falseTarget, _) =>
      val reg = memory.load(cond, Set(), bbName)
      builder.add(CmpRegImm(reg, Imm(0)))
      //since jump is last instruction of basic block, save all variables to stack and clear registers
      memory.cleanDeadInstructions(bbName, instr)
      memory.deallocAllRegs()
      builder.add(Je(LabelOp(falseTarget)))
      builder.add(Jmp(LabelOp(trueTarget)))

    case IRCall(fun, args, _, _) =>
      //realloc register RAX which will contain result of the call
      memory.reallocReg(RAX(X86RegMode.M64).id, Set(), bbName)
      //save current state of registers
      val savedRegs = memory.saveRegs()
      val savedSize = savedRegs.length * 8

      //stack alignment
      val argsSize = args.foldLeft(0)((acc, a) => acc + a.tp.size)
      val aligned = alignStack(savedSize + argsSize)

      //load function
      val funReg = memory.load(fun, Set(), bbName)

      //push call parameters to stack and call the function
      args.foreach(p => {
        val reg = memory.load(p, Set(), bbName)
        p.tp match {
          //composed types are loaded into registers and then pushed on the stack
          case ComposedType(size) =>
            //prepare empty register
            val workRegId = memory.createEmptyReg(Set(p), bbName)
            val workReg = Register(workRegId, memory.getRegMode(p.tp))
            //copy record on stack as a parameter
            (0 until size by 8).foreach(offset => {
              builder.add(MovRegMem(workReg, MemOffset(reg, -offset)))
              builder.add(PushReg(Register(workRegId, X86RegMode.M64)))
            })

          //every other type is copied from register
          case _ =>
            builder.add(PushReg(Register(reg.id, X86RegMode.M64)))
        }
      })

      //finally, call the function
      builder.add(CallReg(funReg))

      //clear parameters from the stack
      builder.add(AddRegImm(RSP(X86RegMode.M64), Imm(argsSize)))
      //dealign stack
      dealignStack(aligned)
      //restore previous state of registers
      memory.loadRegs(savedRegs)
      memory.assignToReg(instr, RAX(X86RegMode.M64))
      //in case of composed type move its value on stack from previous stack frame to current one
      instr.tp match {
        case ComposedType(size) =>
          val offset = memory.allocateTempOnStack(instr)
          //prepare empty register
          val workRegId = memory.createEmptyReg(Set(instr), bbName)
          val workReg = Register(workRegId, memory.getRegMode(instr.tp))
          copyRecord(workReg, RAX(workReg.mode), RBP(X86RegMode.M64), offset, size)
          //update RAX with new address that points to current stack frame
          memAddrToReg(RAX(X86RegMode.M64), RBP(X86RegMode.M64), offset)

        case _ => ()
      }

    case _ =>
      throw new RuntimeException(s"Unsupported x86IR instruction for compilation into x86: $instr")
  }


  //== Stack operations ==

  /** Aligns stack to 16 bytes */
  private def alignStack(stackSize: Int): Int = {
    val remainder = stackSize % 16
    if (remainder == 0) {
      0
    } else {
      val toAlign = 16 - remainder
      builder.add(SubRegImm(RSP(X86RegMode.M64), Imm(toAlign), Some("Align stack to 16 bytes")))
      toAlign
    }
  }

  /** Dealigns stack back */
  private def dealignStack(aligned: Int): Unit = {
    if (aligned > 0) {
      builder.add(AddRegImm(RSP(X86RegMode.M64), Imm(aligned), Some("Dealign stack back")))
    }
  }

  /** Copies record by 8 byte parts from one stack location to another */
  private def copyRecord(workReg: Register, srcReg: Register, destAddr: Addressable, destAddrOffset: Int, size: Int): Unit = {
    //one by one move parts of the composed type
    (0 until size by 8).foreach(offset => {
      builder.add(MovRegMem(workReg, MemOffset(srcReg, -offset)))
      builder.add(MovMemReg(MemOffset(destAddr, destAddrOffset - offset), workReg))
    })
  }

  //== Register helpers ==
  /** Returns correct form of a memory access with or without offset */
  private def getMem(op: Addressable, offset: Int): Memory = offset match {
    case 0 => Mem(op)
    case _ => MemOffset(op, offset)
  }

  /** Loads memory address (with or without offset) into a register */
  private def memAddrToReg(dest: Register, memOp: Addressable, memOffset: Int): Unit = {
    memOp match {
      case r: Register => builder.add(MovRegReg(dest, r))
      case l: LabelOp => builder.add(MovRegLabel(dest, l))
    }
    if (memOffset != 0) {
      builder.add(AddRegImm(dest, Imm(memOffset)))
    }
  }

  //== Label generators ==

  /** Creates label without incrementing label count */
  private def sameLabel(name: String): String = name + "_" + labelCnt

  /** Creates label with incremented label count */
  private def nextLabel(name: String): String = {
    labelCnt += 1
    sameLabel(name)
  }

  //== Other ==

  /** Returns true if the instruction has no side-effect and its value is dead right after it's executed */
  private def isUseless(instr: IRInstruction, bbName: String): Boolean = instr match {
    //only LoadImm has no observable side-effect
    case _: LoadImm => !lia.liveAfterInstr(bbName, instr).contains(instr)
    case _ => false
  }
}
