sealed trait Instruction {
  val tp: IRType
  val loc: Loc
  def operands: Iterable[Instruction]
}