sealed trait Instruction {
  val name: String
  val comment: Option[String]
}

sealed trait ZeroOperand extends Instruction

sealed trait OneOperand extends Instruction {
  val op: Operand
}

sealed trait TwoOperand extends Instruction {
  val dest: Operand
  val src: Operand
}