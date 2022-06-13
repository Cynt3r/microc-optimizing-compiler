package microc.middleend.lattice

sealed trait SignElem

object SignLattice extends FlatLattice[SignElem] {
  case object Pos extends SignElem
  case object Neg extends SignElem
  case object Zer extends SignElem

  private type SignTable = List[List[Sign]]

  private val idx: Map[Sign, Int] =
    List[Sign](Bot, Zer, Neg, Pos, Top).zipWithIndex.toMap

  private def lookup(table: SignTable, a: Sign, b: Sign): Sign =
    table(idx(a))(idx(b))

  private val plusTable: SignTable =
    List(
      List(Bot, Bot, Bot, Bot, Bot),
      List(Bot, Zer, Neg, Pos, Top),
      List(Bot, Neg, Neg, Top, Top),
      List(Bot, Pos, Top, Pos, Top),
      List(Bot, Top, Top, Top, Top)
    )

  private val minusTable: SignTable =
    List(
      List(Bot, Bot, Bot, Bot, Bot),
      List(Bot, Zer, Pos, Neg, Top),
      List(Bot, Neg, Top, Neg, Top),
      List(Bot, Pos, Pos, Top, Top),
      List(Bot, Top, Top, Top, Top)
    )

  private val timesTable: SignTable =
    List(
      List(Bot, Bot, Bot, Bot, Bot),
      List(Bot, Zer, Zer, Zer, Zer),
      List(Bot, Zer, Pos, Neg, Top),
      List(Bot, Zer, Neg, Pos, Top),
      List(Bot, Zer, Top, Top, Top)
    )

  private val divTable: SignTable =
    List(
      List(Bot, Bot, Bot, Bot, Bot),
      List(Bot, Bot, Zer, Zer, Top),
      List(Bot, Bot, Top, Top, Top),
      List(Bot, Bot, Top, Top, Top),
      List(Bot, Bot, Top, Top, Top)
    )

  private val gtTable: SignTable =
    List(
      List(Bot, Bot, Bot, Bot, Bot),
      List(Bot, Zer, Pos, Zer, Top),
      List(Bot, Zer, Top, Zer, Top),
      List(Bot, Pos, Pos, Top, Top),
      List(Bot, Top, Top, Top, Top)
    )

  private val eqqTable: SignTable =
    List(
      List(Bot, Bot, Bot, Bot, Bot),
      List(Bot, Pos, Zer, Zer, Top),
      List(Bot, Zer, Top, Zer, Top),
      List(Bot, Zer, Zer, Top, Top),
      List(Bot, Top, Top, Top, Top)
    )

  def plus(a: Sign, b: Sign): Sign = lookup(plusTable, a, b)
  def minus(a: Sign, b: Sign): Sign = lookup(minusTable, a, b)
  def times(a: Sign, b: Sign): Sign = lookup(timesTable, a, b)
  def div(a: Sign, b: Sign): Sign = lookup(divTable, a, b)
  def eqq(a: Sign, b: Sign): Sign = lookup(eqqTable, a, b)
  def gt(a: Sign, b: Sign): Sign = lookup(gtTable, a, b)

  def num(n: Int): Sign = n match {
    case 0          => Zer
    case _ if n > 0 => Pos
    case _          => Neg
  }
}
