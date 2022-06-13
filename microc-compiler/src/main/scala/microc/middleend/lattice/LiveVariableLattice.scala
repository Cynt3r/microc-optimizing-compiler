package microc.middleend.lattice

import microc.frontend.ast.Decl

class LiveVariableLattice(every: Set[Decl]) extends Lattice[Set[Decl]] {
  override def top: Set[Decl] = every

  override def bot: Set[Decl] = Set()

  override def lub(x: Set[Decl], y: Set[Decl]): Set[Decl] = x.union(y)
}
