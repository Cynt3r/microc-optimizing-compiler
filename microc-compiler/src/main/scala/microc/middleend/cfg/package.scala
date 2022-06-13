package microc.middleend

import microc.frontend.ast._

import scala.collection.mutable

package object cfg {

  type CfgNodeContexts = mutable.Map[CfgNode, CfgNodeContext]

  implicit class CfgStmtContextsOps(contexts: CfgNodeContexts) {
    def merge(that: CfgNodeContexts): CfgNodeContexts = {
      that.foldLeft(contexts)((merged, pair) => merged + pair)
    }
  }

  implicit class CfgNodeOps(node: CfgNode) {
    def declaredVars: Set[Decl] =
      node match {
        case CfgStmtNode(VarStmt(ids, _)) => ids.toSet
        case CfgFunEntryNode(FunDecl(_, params, _, _)) => params.toSet
        case _ => Set()
      }
  }

}
