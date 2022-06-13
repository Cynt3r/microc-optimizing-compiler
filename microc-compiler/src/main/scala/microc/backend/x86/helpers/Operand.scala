package microc.backend.x86.helpers

import microc.backend.x86.helpers.X86RegMode.X86RegMode

/** Instruction operand */
sealed trait Operand

/** Address operand */
sealed trait Addressable extends Operand

/**
 * Immediate value
 * @param value integer value
 */
case class Imm(value: Int) extends Operand

/**
 * Label operand
 * @param name name of the label
 */
case class LabelOp(name: String) extends Addressable

/** Abstract memory access operand */
sealed trait Memory extends Operand

/**
 * Memory access operand with no offset
 * @param op operand with the address
 */
case class Mem(op: Addressable) extends Memory

/**
 * Memory access operand with an offset
 * @param op operand with the address
 * @param offset address offset
 */
case class MemOffset(op: Addressable, offset: Int) extends Memory

/** Enum of x86-64 register modes */
object X86RegMode extends Enumeration {
  type X86RegMode = Value

  val M8: X86RegMode.Value = Value(1)
  val M16: X86RegMode.Value = Value(2)
  val M32: X86RegMode.Value = Value(4)
  val M64: X86RegMode.Value = Value(8)
}

/** x86-64 register */
sealed trait Register extends Addressable {
  /** Id of the register (0 to 15 inclusive) */
  val id: Int

  /** Name of the register */
  val name: String

  /** Access mode of the register */
  val mode: X86RegMode
}

object Register {

  /** Constructs register for given id and register mode */
  def apply(id: Int, mode: X86RegMode): Register = {
    id match {
      case 0 => RAX(mode)
      case 1 => RBX(mode)
      case 2 => RCX(mode)
      case 3 => RDX(mode)
      case 4 => RSI(mode)
      case 5 => RDI(mode)
      case 6 => RBP(mode)
      case 7 => RSP(mode)
      case 8 => R8(mode)
      case 9 => R9(mode)
      case 10 => R10(mode)
      case 11 => R11(mode)
      case 12 => R12(mode)
      case 13 => R13(mode)
      case 14 => R14(mode)
      case 15 => R15(mode)
      case _ => throw new RuntimeException("Unknown id of a register")
    }
  }

  /** Returns list of ids of all general purpose registers */
  def generalPurpose: List[Int] = List(0, 1, 2, 3, 4, 5, 8, 9, 10, 11, 12, 13, 14, 15)

  /** Returns list of ids of caller saved registers */
  def callerSaved: List[Int] = List(0, 2, 3, 4, 5, 8, 9, 10, 11)
}

case class RAX(mode: X86RegMode) extends Register {
  val id: Int = 0

  val name: String = mode match {
    case X86RegMode.M8 => "al"
    case X86RegMode.M16 => "ax"
    case X86RegMode.M32 => "eax"
    case X86RegMode.M64 => "rax"
  }
}

case class RBX(mode: X86RegMode) extends Register {
  val id: Int = 1

  val name: String = mode match {
    case X86RegMode.M8 => "bl"
    case X86RegMode.M16 => "bx"
    case X86RegMode.M32 => "ebx"
    case X86RegMode.M64 => "rbx"
  }
}

case class RCX(mode: X86RegMode) extends Register {
  val id: Int = 2

  val name: String = mode match {
    case X86RegMode.M8 => "cl"
    case X86RegMode.M16 => "cx"
    case X86RegMode.M32 => "ecx"
    case X86RegMode.M64 => "rcx"
  }
}

case class RDX(mode: X86RegMode) extends Register {
  val id: Int = 3

  val name: String = mode match {
    case X86RegMode.M8 => "dl"
    case X86RegMode.M16 => "dx"
    case X86RegMode.M32 => "edx"
    case X86RegMode.M64 => "rdx"
  }
}

case class RSI(mode: X86RegMode) extends Register {
  val id: Int = 4

  val name: String = mode match {
    case X86RegMode.M8 => "sil"
    case X86RegMode.M16 => "si"
    case X86RegMode.M32 => "esi"
    case X86RegMode.M64 => "rsi"
  }
}

case class RDI(mode: X86RegMode) extends Register {
  val id: Int = 5

  val name: String = mode match {
    case X86RegMode.M8 => "dil"
    case X86RegMode.M16 => "di"
    case X86RegMode.M32 => "edi"
    case X86RegMode.M64 => "rdi"
  }
}

case class RBP(mode: X86RegMode) extends Register {
  val id: Int = 6

  val name: String = mode match {
    case X86RegMode.M8 => "bpl"
    case X86RegMode.M16 => "bp"
    case X86RegMode.M32 => "ebp"
    case X86RegMode.M64 => "rbp"
  }
}

case class RSP(mode: X86RegMode) extends Register {
  val id: Int = 7

  val name: String = mode match {
    case X86RegMode.M8 => "spl"
    case X86RegMode.M16 => "sp"
    case X86RegMode.M32 => "esp"
    case X86RegMode.M64 => "rsp"
  }
}

case class R8(mode: X86RegMode) extends Register {
  val id: Int = 8

  val name: String = mode match {
    case X86RegMode.M8 => "r8b"
    case X86RegMode.M16 => "r8w"
    case X86RegMode.M32 => "r8d"
    case X86RegMode.M64 => "r8"
  }
}

case class R9(mode: X86RegMode) extends Register {
  val id: Int = 9

  val name: String = mode match {
    case X86RegMode.M8 => "r9b"
    case X86RegMode.M16 => "r9w"
    case X86RegMode.M32 => "r9d"
    case X86RegMode.M64 => "r9"
  }
}

case class R10(mode: X86RegMode) extends Register {
  val id: Int = 10

  val name: String = mode match {
    case X86RegMode.M8 => "r10b"
    case X86RegMode.M16 => "r10w"
    case X86RegMode.M32 => "r10d"
    case X86RegMode.M64 => "r10"
  }
}

case class R11(mode: X86RegMode) extends Register {
  val id: Int = 11

  val name: String = mode match {
    case X86RegMode.M8 => "r11b"
    case X86RegMode.M16 => "r11w"
    case X86RegMode.M32 => "r11d"
    case X86RegMode.M64 => "r11"
  }
}

case class R12(mode: X86RegMode) extends Register {
  val id: Int = 12

  val name: String = mode match {
    case X86RegMode.M8 => "r12b"
    case X86RegMode.M16 => "r12w"
    case X86RegMode.M32 => "r12d"
    case X86RegMode.M64 => "r12"
  }
}

case class R13(mode: X86RegMode) extends Register {
  val id: Int = 13

  val name: String = mode match {
    case X86RegMode.M8 => "r13b"
    case X86RegMode.M16 => "r13w"
    case X86RegMode.M32 => "r13d"
    case X86RegMode.M64 => "r13"
  }
}

case class R14(mode: X86RegMode) extends Register {
  val id: Int = 14

  val name: String = mode match {
    case X86RegMode.M8 => "r14b"
    case X86RegMode.M16 => "r14w"
    case X86RegMode.M32 => "r14d"
    case X86RegMode.M64 => "r14"
  }
}

case class R15(mode: X86RegMode) extends Register {
  val id: Int = 15

  val name: String = mode match {
    case X86RegMode.M8 => "r15b"
    case X86RegMode.M16 => "r15w"
    case X86RegMode.M32 => "r15d"
    case X86RegMode.M64 => "r15"
  }
}
