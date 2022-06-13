package microc.frontend.util

import scala.collection.mutable

class UnionFind[T] {

  private val parents = mutable.Map[T, T]()

  def graph: Map[T, T] = parents.toMap

  def makeSet(x: T): T = {
    if (!parents.contains(x)) {
      parents += x -> x
    }
    x
  }

  def find(x: T): T = {
    if (parents(x) != x) {
      parents(x) = find(parents(x))
    }
    parents(x)
  }

  def union(x: T, y: T): T = {
    parents(x) = y
    y
  }

}
