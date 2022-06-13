package microc.middleend.lattice

class MapLattice[A, B](set: A => Boolean, val lat: Lattice[B]) extends Lattice[Map[A, B]] {
  override def top: Map[A, B] = Map().withDefaultValue(lat.top)

  override def bot: Map[A, B] = Map().withDefaultValue(lat.bot)

  override def lub(x: Map[A, B], y: Map[A, B]): Map[A, B] =
    x.keys.foldLeft(y)((acc, elem) => acc + (elem -> lat.lub(x(elem), y(elem))))
}
