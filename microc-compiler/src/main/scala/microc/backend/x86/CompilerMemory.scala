package microc.backend.x86

import microc.backend.x86.helpers.X86RegMode.X86RegMode
import microc.backend.x86.helpers._
import microc.backend.x86IR.{Alloc, ComposedType, FunAlloc, IRType, PointType, SimpleType, VoidType, Instruction => IRInstruction}

import scala.annotation.tailrec

/** Representation of an x86 machine memory */
class CompilerMemory(analysis: LiveInstructionAnalysis, builder: X86Builder) {
  /** Represents registers (their ids) of the machine and maps instructions to them */
  private var registers: Map[Int, Option[IRInstruction]] = Register.generalPurpose.map((_, None)).toMap
  /** Represents part of a stack frame that contains locals and arguments and maps them to their offset */
  private var variables: Map[Alloc, Int] = Map()
  /** Represents part of a stack frame that contains temporary values and maps them to their offset */
  private var temps: Map[IRInstruction, Int] = Map()
  /** Total bytesize of arguments */
  private var argsSize: Int = 0
  /** Total bytesize of locals */
  private var localsSize: Int = 0
  /** Total bytesize of temporary values */
  private var tempsSize: Int = 0

  //== Stack operations ==

  /** Allocates a new stack frame */
  def allocFrame(aSize: Int, lSize: Int): Unit = {
    argsSize = aSize
    localsSize = lSize
    tempsSize = 0
  }

  /** Registers a new local variable on the stack */
  def addLocal(alloc: Alloc): Unit = {
    //size of all already added locals
    val sizeSum = variables
      .filter(x => x._2 <= 0) //filter locals
      .map(_._1.tp.pointsTo.size) //get sizes of locals
      .reduceOption(_ + _) //get sum of all sizes
      .getOrElse(0)
    val startOffset = -8
    setVariable(startOffset - sizeSum, alloc)
  }

  /** Registers a new function argument on the stack */
  def addArg(alloc: Alloc): Unit = {
    //size of all already added arguments
    val sizeSum = variables
      .filter(x => x._2 > 0) //filter arguments
      .map(_._1.tp.pointsTo.size) //get sizes of arguments
      .reduceOption(_ + _) //get sum of all sizes
      .getOrElse(0)
    val startOffset = argsSize + 8
    setVariable(startOffset - sizeSum, alloc)
  }

  /** Finds memory location of the given instruction */
  def findMemLoc(instr: IRInstruction, dontSpill: Set[IRInstruction], bbName: String): (Addressable, Int) = instr match {
    case alloc: Alloc => (RBP(X86RegMode.M64), variables(alloc))
    case FunAlloc(fun, _) => (LabelOp(builder.funMemLocation(fun.name)), 0)
    case _ => (load(instr, dontSpill, bbName), 0)
  }

  /** Allocates a new temporary value on the stack and returns its offset */
  def allocateTempOnStack(instr: IRInstruction): Int = {
    //find the next offset for the instruction
    val offset = temps match {
      case _ if temps.isEmpty => - (localsSize + 8) //starting offset is a negative size of locals + 8 (since locals start from offset 8)
      case _ =>
        val lastTemp = temps.reduce((t1, t2) => if (t1._2 < t2._2) t1 else t2) //last temporary
        lastTemp._2 - lastTemp._1.tp.size //next offset is offset of a last temporary minus its size
    }
    setTemp(offset, instr)
    offset
  }

  /** Clears current stack frame and returns size of temporary values on the stack */
  def clearFrame(): Int = {
    variables = Map()
    temps = Map()
    deallocAllRegs()
    tempsSize
  }


  //== Register operations ==

  /** Returns register where the instruction is stored or loads it from stack to some register if necessary */
  def load(instr: IRInstruction, dontSpill: Set[IRInstruction], bbName: String): Register = {
    findInstrReg(instr) match {
      case Some(reg) => reg

      //instruction needs to be loaded from stack
      case None =>
        val reg = allocReg(instr, dontSpill, bbName)
        loadTempFromStack(instr, reg)
        reg
    }
  }

  /** Assigns instruction to a register */
  def allocReg(instr: IRInstruction, dontSpill: Set[IRInstruction], bbName: String): Register = {
    //find or prepare empty register
    val regId = createEmptyReg(dontSpill, bbName)
    val reg = Register(regId, getRegMode(instr.tp))
    assignToReg(instr, reg)
    reg
  }

  /** Finds or creates an empty register and returns its id */
  def createEmptyReg(dontSpill: Set[IRInstruction], bbName: String): Int = {
    findEmptyReg.getOrElse(spill(dontSpill, bbName))
  }

  /** Reallocates instruction from given register into a different one if necessary (used for instructions, that require specific registers, e.g. IDIV) */
  def reallocReg(regId: Int, dontSpill: Set[IRInstruction], bbName: String): Unit = {
    registers(regId).foreach(instr => {
      //temporally remove register from register pool
      registers = registers - regId
      //allocate new register for instruction
      val regNew = allocReg(instr, dontSpill, bbName)
      builder.add(MovRegReg(regNew, Register(regId, getRegMode(instr.tp))))
      //add previously removed register to the register pool
      registers = registers + (regId -> None)
    })
  }

  /** Assigns instruction to a register without spilling */
  def assignToReg(instr: IRInstruction, reg: Register): Unit = {
    registers = registers + (reg.id -> Some(instr))
  }

  /** Clears all local registers */
  def deallocAllRegs(): Unit = Register.generalPurpose.foreach(deallocReg)

  /** Saves registers that contain instruction onto stack and returns list of ids of saved registers */
  def saveRegs(): List[Int] = saveSelectedRegs(Register.generalPurpose)

  /** Saves caller saved registers (without the registers in except list) that contain instruction to the stack and returns list of ids of saved registers */
  def saveScratchRegs(except: List[Register]): List[Int] = {
    val toSave = Register.callerSaved.filterNot(except.map(_.id).contains(_))
    saveSelectedRegs(toSave)
  }

  /** Pops values from the stack into registers */
  def loadRegs(regs: List[Int]): Unit = regs.reverse.foreach(id => builder.add(PopReg(Register(id, X86RegMode.M64))))

  /** If instruction is considered dead, it gets removed from the registers and temporary memory on the stack */
  def cleanDeadInstructions(bbName: String, instr: IRInstruction): Unit = {
    val live = analysis.liveAfterInstr(bbName, instr)
    //clean registers
    registers
      .filter {
        case (_, Some(ins)) if !live.contains(ins) => true
        case _ => false
      } //filter dead registers
      .foreach(reg => deallocReg(reg._1))

    //clean temporary values
    temps
      .filter {
        case (ins, _) if !live.contains(ins) => true
        case _ => false
      }
      .foreach(t => temps = temps - t._1)
  }

  /** Returns correct register mode for given IRType */
  def getRegMode(tp: IRType): X86RegMode = tp match {
    case VoidType => X86RegMode.M8 //shouldn't be used
    case _: PointType => X86RegMode.M64 //size of an address is 64bits
    case SimpleType => X86RegMode.M32 //size of an integer is 32bits
    case _: ComposedType => X86RegMode.M64 //size of an address of a record is 64bits
  }

  /** Loads temporary value from the stack to a register */
  private def loadTempFromStack(instr: IRInstruction, reg: Register): Unit = {
    //find stack offset of the instruction
    val offset = temps(instr)
    //generate load operation
    builder.add(MovRegMem(reg, MemOffset(RBP(X86RegMode.M64), offset)))
    //temporary values are removed from stack after load
    temps = temps - instr
  }

  /** Spills temporary value to the stack */
  private def spillToStack(instr: IRInstruction, regId: Int): Unit = {
    //find the next offset for the instruction
    val offset = allocateTempOnStack(instr)
    //generate spill operation
    builder.add(MovMemReg(MemOffset(RBP(X86RegMode(instr.tp.size)), offset), Register(regId, getRegMode(instr.tp))))
  }

  /** Assigns variable to the given offset */
  private def setVariable(offset: Int, alloc: Alloc): Unit = {
    variables = variables + (alloc -> offset)
  }

  /** Assigns temporary value to the given offset */
  private def setTemp(offset: Int, instr: IRInstruction): Unit = {
    temps = temps + (instr -> offset)
    val currTempsSize = -(offset - instr.tp.size) - (localsSize + 8)
    //update size of temps
    tempsSize = tempsSize.max(currTempsSize)
  }

  /** Clears logical register */
  private def deallocReg(regId: Int): Unit = {
    registers = registers + (regId -> None)
  }

  /** Finds register in which the instruction is stored */
  private def findInstrReg(instr: IRInstruction): Option[Register] = {
    registers
      .find(_._2.contains(instr))
      .map(_._1) //get reg id
      .map(Register(_, getRegMode(instr.tp))) //map it to the correct register mode
  }

  /** Finds id of a register not containing any instruction (if such register exists) */
  private def findEmptyReg: Option[Int] = {
    registers.find(_._2.isEmpty).map(_._1)
  }

  /** Spills instruction that is not in the "dontSpill" set and will be used furthest in the future */
  private def spill(dontSpill: Set[IRInstruction], bbName: String): Int = {
    val spillable = registers.collect {
      case (regId, Some(instr)) if !dontSpill.contains(instr) => (regId, instr)
    }
    val usageOrder = analysis.usageOrder(bbName)

    //find register to spill
    val (regId, instr) = getLeastUsed(spillable, usageOrder)
    deallocReg(regId)
    spillToStack(instr, regId)
    regId
  }

  /** Saves registers from given list that contain instruction to the stack and returns list of saved registers */
  private def saveSelectedRegs(regIds: List[Int]): List[Int] = {
    val savedRegs = regIds.filter(registers(_).nonEmpty)
    savedRegs.foreach(id => builder.add(PushReg(Register(id, X86RegMode.M64))))
    savedRegs
  }

  @tailrec
  /** Returns least used instruction (i.e., instruction that will be used furthest in the future) and its register */
  private def getLeastUsed(spillable: Map[Int, IRInstruction], order: List[IRInstruction]): (Int, IRInstruction) =  order match {
    case _ if spillable.isEmpty => throw new RuntimeException("Internal error, somehow ran out of registers that can be spilled")
    case _ if spillable.size == 1 => spillable.head
    case Nil => spillable.head
    case next :: rest => getLeastUsed(spillable.filter(_._2 != next), rest)
  }
}
