package microc.middleend.lattice

import scala.language.implicitConversions

trait Lattice[A] {

  /** Returns the ⊤ element */
  def top: A

  /** Returns the ⊥ element */
  def bot: A

  /** Return the least upper bound ⨆{x,y} (`x` ⊔ `y`) */
  def lub(x: A, y: A): A
}

sealed trait FlatElem[+A]

case object FlatTop extends FlatElem[Nothing] {
  override def toString: String = "⊤"
}
case object FlatBot extends FlatElem[Nothing] {
  override def toString: String = "⊥"
}
case class FlatVal[+A](elem: A) extends FlatElem[A] {
  override def toString: String = elem.toString
}

class FlatLattice[A] extends Lattice[FlatElem[A]] {
  val Top = FlatTop

  val Bot = FlatBot

  override def top: FlatElem[A] = Top

  override def bot: FlatElem[A] = Bot

  override def lub(x: FlatElem[A], y: FlatElem[A]): FlatElem[A] = (x, y) match {
    case (Bot, y)         => y
    case (x, Bot)         => x
    case (Top, _)         => Top
    case (_, Top)         => Top
    case (x, y) if x == y => x
    case _                => Top
  }

  implicit def wrap(v: A): FlatElem[A] = FlatVal(v)
}
