package microc.backend.x86.helpers

sealed trait Instruction {
  /** Serializes the name of the instruction to NASM format string */
  val name: String

  /** Comment associated with the instruction */
  val comment: Option[String]
}


//== Abstract instructions

sealed trait ZeroOperand extends Instruction

sealed trait OneOperand extends Instruction {
  /** Operand of the instruction */
  val op: Operand
}

sealed trait TwoOperand extends Instruction {
  /** Destination operand of the instruction */
  val dest: Operand

  /** Source operand of the instruction */
  val src: Operand
}


//== Zero operand instructions

case class Label(value: String, comment: Option[String] = None) extends ZeroOperand {
  val name: String = value + ":"
}

case class Ret(comment: Option[String] = None) extends ZeroOperand {
  val name: String = "ret"
}


//== One operand instructions

sealed trait Call extends OneOperand {
  val name: String = "call"
}
case class CallLabel(op: LabelOp, comment: Option[String] = None) extends Call
case class CallReg(op: Register, comment: Option[String] = None) extends Call

case class PushReg(op: Register, comment: Option[String] = None) extends OneOperand {
  val name: String = "push"
}

case class PopReg(op: Register, comment: Option[String] = None) extends OneOperand {
  val name: String = "pop"
}

case class Jmp(op: LabelOp, comment: Option[String] = None) extends OneOperand {
  val name: String = "jmp"
}

case class Jne(op: LabelOp, comment: Option[String] = None) extends OneOperand {
  val name: String = "jne"
}

case class Je(op: LabelOp, comment: Option[String] = None) extends OneOperand {
  val name: String = "je"
}

case class Jle(op: LabelOp, comment: Option[String] = None) extends OneOperand {
  val name: String = "jle"
}

sealed trait Div extends OneOperand {
  val name: String = "idiv"
}
case class DivImm(op: Imm, comment: Option[String] = None) extends Div
case class DivReg(op: Register, comment: Option[String] = None) extends Div

case class Inc(op: Register, comment: Option[String] = None) extends OneOperand {
  val name: String = "inc"
}

case class Dec(op: Register, comment: Option[String] = None) extends OneOperand {
  val name: String = "dec"
}


//== Two operand instructions

sealed trait Add extends TwoOperand {
  val name: String = "add"
}
case class AddRegImm(dest: Register, src: Imm, comment: Option[String] = None) extends Add
case class AddRegReg(dest: Register, src: Register, comment: Option[String] = None) extends Add

sealed trait Sub extends TwoOperand {
  val name: String = "sub"
}
case class SubRegImm(dest: Register, src: Imm, comment: Option[String] = None) extends Sub
case class SubRegReg(dest: Register, src: Register, comment: Option[String] = None) extends Sub

sealed trait Mul extends TwoOperand {
  val name: String = "imul"
}
case class MulRegImm(dest: Register, src: Imm, comment: Option[String] = None) extends Mul
case class MulRegReg(dest: Register, src: Register, comment: Option[String] = None) extends Mul

case class XorRegReg(dest: Register, src: Register, comment: Option[String] = None) extends TwoOperand {
  val name: String = "xor"
}

case class ShlRegImm(dest: Register, src: Imm, comment: Option[String] = None) extends TwoOperand {
  val name: String = "shl"
}

case class ShrRegImm(dest: Register, src: Imm, comment: Option[String] = None) extends TwoOperand {
  val name: String = "shr"
}

sealed trait Cmp extends TwoOperand {
  val name: String = "cmp"
}
case class CmpRegReg(dest: Register, src: Register, comment: Option[String] = None) extends Cmp
case class CmpRegImm(dest: Register, src: Imm, comment: Option[String] = None) extends Cmp

sealed trait Mov extends TwoOperand {
  val name: String = "mov"
}
case class MovRegImm(dest: Register, src: Imm, comment: Option[String] = None) extends Mov
case class MovRegReg(dest: Register, src: Register, comment: Option[String] = None) extends Mov
case class MovMemReg(dest: Memory, src: Register, comment: Option[String] = None) extends Mov
case class MovRegMem(dest: Register, src: Memory, comment: Option[String] = None) extends Mov
case class MovRegLabel(dest: Register, src: LabelOp, comment: Option[String] = None) extends Mov

case class MovsxRegReg(dest: Register, src: Register, comment: Option[String] = None) extends TwoOperand {
  val name: String = "movsx"
}
