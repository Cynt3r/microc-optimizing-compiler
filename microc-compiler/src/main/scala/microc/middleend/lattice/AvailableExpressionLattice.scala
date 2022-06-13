package microc.middleend.lattice

import microc.frontend.ast.Expr

class AvailableExpressionLattice(every: Set[Expr]) extends Lattice[Set[Expr]] {
  override def top: Set[Expr] = Set()

  override def bot: Set[Expr] = every

  override def lub(x: Set[Expr], y: Set[Expr]): Set[Expr] = x.intersect(y)
}
