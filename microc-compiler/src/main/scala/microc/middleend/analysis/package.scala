package microc.middleend

import microc.frontend.ast._
import microc.middleend.cfg.CfgNode
import microc.middleend.lattice._

package object analysis {

  type Declarations = Map[Identifier, Decl]

  type Types = Map[Decl, Type]

  type Signs = Map[CfgNode, Map[Decl, Sign]]

  type Constants = Map[CfgNode, Map[Decl, Constant]]

  type LiveVars = Map[CfgNode, Set[Decl]]

  type AvailableExps = Map[CfgNode, Set[Expr]]
}
