package microc.middleend.lattice

object ConstantPropagationLattice extends FlatLattice[Int] {

  private def apply(op: (Int, Int) => Constant, a: Constant, b: Constant): Constant = (a, b) match {
    case (FlatVal(x), FlatVal(y)) => op(x, y)
    case (Bot, _)                 => Bot
    case (_, Bot)                 => Bot
    case (_, Top)                 => Top
    case (Top, _)                 => Top
  }

  def num(i: Int): Constant = FlatVal(i)

  def plus(a: Constant, b: Constant): Constant = apply(_ + _, a, b)

  def minus(a: Constant, b: Constant): Constant = apply(_ - _, a, b)

  def times(a: Constant, b: Constant): Constant = apply(_ * _, a, b)

  def div(a: Constant, b: Constant): Constant = apply((x, y) => if (y != 0) x / y else Bot, a, b)

  def eqq(a: Constant, b: Constant): Constant = apply((x, y) => if (x == y) 1 else 0, a, b)

  def gt(a: Constant, b: Constant): Constant = apply((x, y) => if (x > y) 1 else 0, a, b)

}
