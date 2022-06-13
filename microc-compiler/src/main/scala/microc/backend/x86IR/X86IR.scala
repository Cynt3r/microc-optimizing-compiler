package microc.backend.x86IR

import microc.frontend.ast.{FunDecl, IdentifierDecl, Loc}
import microc.middleend.cfg.CfgNode

/**
 * Represents x86IR program
 * @param functions list of functions of the program
 */
case class ProgramX86IR(functions: List[Function]) {
  def mainFunction: Function = {
    functions.find(_.name == "main").getOrElse(throw new RuntimeException("Undefined main function"))
  }
}

/**
 * Represents function (sequence of basic blocks)
 * @param name name of the function
 * @param paramCnt number of parameters of the function
 * @param bbs list of basic blocks of the function
 */
case class Function(name: String, paramCnt: Int, bbs: List[BasicBlock]) {
  /** Returns local variables of the function */
  def getLocals: List[Alloc] = {
    bbs
      .flatMap(_.instructions)
      .foldLeft(Set.empty[Alloc]) {
        case (acc: Set[Alloc], alloc: Alloc) => acc + alloc
        case (acc: Set[Alloc], _) => acc
      }
      .toList
  }

  /** Returns parameters of the function */
  def getParams: List[Alloc] = {
    bbs
      .flatMap(_.instructions)
      .foldLeft(Set.empty[Alloc]) {
        case (acc: Set[Alloc], ArgAddr(alloc, _)) => acc + alloc
        case (acc: Set[Alloc], _) => acc
      }
      .toList
  }
}

/**
 * Represents a basic block (sequence of instructions without control flow)
 * @param name name of the basic block
 * @param instructions list of instructions of the basic block
 * @param sourceCfgNodes map that maps instructions of the basic block to their source CFG node
 */
case class BasicBlock(name: String, instructions: List[Instruction], sourceCfgNodes: Map[Instruction, CfgNode]) {
  def instructionSrc(instr: Instruction): CfgNode = sourceCfgNodes(instr)
}

/** Type of the IR instruction */
sealed trait IRType {
  /** Byte-size of the type */
  def size: Int
}

/** Type of an instruction that yields no value */
case object VoidType extends IRType {
  def size: Int = 0
}

/** Pointer type */
case class PointType(pointsTo: IRType) extends IRType {
  def size: Int = 8
}

/** Non-composed simple type (e.g., integer, function label) */
case object SimpleType extends IRType {
  def size: Int = 8
}

/** Composed type, e.g., Record */
case class ComposedType(size: Int) extends IRType

/** Represents x86 IR instruction */
sealed trait Instruction {
  /** Type of the instruction */
  val tp: IRType
  
  /** Location of the instruction */
  val loc: Loc

  /** Operands of the instruction */
  def operands: Iterable[Instruction]
}

/** Instruction, that terminates basic block (every basic block has to end with this instruction and every terminator has to be the last instruction of the basic block) */
sealed trait Terminator extends Instruction {
  /** IR type (always VoidType) */
  val tp: IRType = VoidType
}

/** Binary operator */
sealed trait BinOperator

case object Plus extends BinOperator

case object Minus extends BinOperator

case object Times extends BinOperator

case object Divide extends BinOperator

case object Gt extends BinOperator

case object Eq extends BinOperator

/**
 * Local variable allocation
 * @param decl declaration of the variable
 * @param tp IR type (always PointType which points to the type of the allocated variable)
 * @param loc location
 */
case class Alloc(decl: IdentifierDecl, tp: PointType, loc: Loc) extends Instruction {
  def operands: Iterable[Instruction] = List()
}

/**
 * Function allocation
 * @param decl declaration of the function
 * @param loc location
 */
case class FunAlloc(decl: FunDecl, loc: Loc) extends Instruction {
  def operands: Iterable[Instruction] = List()

  /** IR type (always a PointType that points to function's address) */
  val tp: IRType = PointType(PointType(SimpleType))
}

/**
 * Dynamic allocation on a heap
 * @param value initial value
 * @param loc location
 */
case class HeapAlloc(value: Instruction, loc: Loc) extends Instruction {
  def operands: Iterable[Instruction] = List(value)

  /** IR type (always PointType that points to the type of the value) */
  val tp: IRType = PointType(value.tp)
}

/**
 * Argument of a function (wraps `Alloc`)
 * @param alloc allocation of the argument
 * @param loc location
 */
case class ArgAddr(alloc: Alloc, loc: Loc) extends Instruction {
  def operands: Iterable[Instruction] = List()

  /** IR type (always same as the alloc) */
  val tp: IRType = alloc.tp
}

/**
 * Address of an instruction
 * @param target instruction whose address the instruction represents
 * @param loc location
 */
case class GetAddr(target: Instruction, loc: Loc) extends Instruction {
  def operands: Iterable[Instruction] = List()

  /** IR type (always PointType that points to type of the target) */
  val tp: IRType = PointType(target.tp)
}

/**
 * Offset of an address
 * @param addr address
 * @param offset offset of the address
 * @param tp IR type (always PointType)
 * @param loc location
 */
case class GetAddrOffset(addr: Instruction, offset: Instruction, tp: PointType, loc: Loc) extends Instruction {
  def operands: Iterable[Instruction] = List(addr, offset)
}

/**
 * Load of an address
 * @param src source address (has to be instruction with type PointType)
 * @param loc location
 */
case class Load(src: Instruction, loc: Loc) extends Instruction {
  require(src.tp.isInstanceOf[PointType])

  def operands: Iterable[Instruction] = List(src)

  /** IR type (always same as the type that src points to) */
  val tp: IRType = src.tp.asInstanceOf[PointType].pointsTo
}

/**
 * An integer
 * @param value number
 * @param tp IRType
 * @param loc location
 */
case class LoadImm(value: Int, tp: IRType, loc: Loc) extends Instruction {
  def operands: Iterable[Instruction] = List()
}

/**
 * Input from stdin
 * @param loc location
 */
case class LoadInput(loc: Loc) extends Instruction {
  def operands: Iterable[Instruction] = List()

  /** IR type (always SimpleType) */
  val tp: IRType = SimpleType
}

/**
 * Composed value
 * @param fields fields of the composed values
 * @param tp IR type (always ComposedType)
 * @param loc location
 */
case class LoadComposed(fields: List[Instruction], tp: ComposedType, loc: Loc) extends Instruction {
  def operands: Iterable[Instruction] = fields
}

/**
 * Assigment from one place to another
 * @param dest destination of the assign
 * @param src source of the assign
 * @param loc location
 */
case class Store(dest: Instruction, src: Instruction, loc: Loc) extends Instruction {
  def operands: Iterable[Instruction] = List(dest, src)

  /** IR type (always VoidType) */
  val tp: IRType = VoidType
}

/**
 * Binary operation (add, sub, etc.)
 * @param op operator
 * @param lhs left side of the operation
 * @param rhs right side of the operation
 * @param loc location
 */
case class BinOp(op: BinOperator, lhs: Instruction, rhs: Instruction, tp: IRType, loc: Loc) extends Instruction {
  def operands: Iterable[Instruction] = List(lhs, rhs)
}

/**
 * Call of a function
 * @param fun function to be called
 * @param args arguments of the function
 * @param tp IR type
 * @param loc location
 */
case class Call(fun: Instruction, args: List[Instruction], tp: IRType, loc: Loc) extends Instruction  {
  def operands: Iterable[Instruction] = fun :: args
}

/**
 * Print of a target value
 * @param target value to be printed
 * @param loc location
 */
case class Print(target: Instruction, loc: Loc) extends Instruction  {
  def operands: Iterable[Instruction] = List(target)

  /** IR type (always VoidType) */
  val tp: IRType = VoidType
}

/**
 * Return from a function
 * @param value return value
 * @param loc location
 */
case class Return(value: Instruction, loc: Loc) extends Terminator {
  def operands: Iterable[Instruction] = List(value)
}

/**
 * Conditional jump to labels
 * @param cond condition of the jump
 * @param trueTarget target label if the condition is true
 * @param falseTarget target label if the condition is false
 * @param loc location
 */
case class CondJump(cond: Instruction, trueTarget: String, falseTarget: String, loc: Loc) extends Terminator {
  def operands: Iterable[Instruction] = List(cond)
}

/**
 * Unconditional jump to a label
 * @param target target label
 * @param loc location
 */
case class Jump(target: String, loc: Loc) extends Terminator {
  def operands: Iterable[Instruction] = List()
}
