package microc.middleend.cfg

import microc.frontend.ast._

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object CfgNode {
  private[cfg] val nextUidCounter = new AtomicInteger(0)

  def nextId: Int = nextUidCounter.getAndIncrement()
}

trait CfgNode {

  val id: Int = CfgNode.nextId

  val pred: mutable.Set[CfgNode] = mutable.Set()

  val succ: mutable.Set[CfgNode] = mutable.Set()

  def ast: AstNode

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case that: CfgNode => that.id == this.id
      case _             => false
    }

  override def hashCode(): Int = id
}

case class CfgStmtNode(
    ast: AstNode
) extends CfgNode {
  override def toString: String = s"[stmt] $ast"
}

case class CfgFunEntryNode(
    ast: FunDecl
) extends CfgNode {

  override def toString: String = s"[fun-entry] $ast"
}

case class CfgFunExitNode(
    ast: FunDecl
) extends CfgNode {

  override def toString: String = s"[fun-exit] $ast"
}
