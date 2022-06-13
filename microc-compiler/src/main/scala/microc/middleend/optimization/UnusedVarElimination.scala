package microc.middleend.optimization

import microc.frontend.ast.{Identifier, IdentifierDecl, VarStmt}
import microc.middleend.cfg.{CfgStmtNode, FunctionCfg, ProgramCfg}

/** Optimization that eliminates unused declarations of variables */
class UnusedVarElimination extends Optimization {
  val cost: Int = 1

  //always runnable, since no analysis is required
  def isRunnable: Boolean = true

  def run(cfg: ProgramCfg): List[OptimizationAction] = {
    cfg.functionsCfg
      .toList
      .flatMap(collectFromFun)
  }

  private def collectFromFun(fun: FunctionCfg): List[OptimizationAction] = {
    fun.nodes
      .toList
      .flatMap { //get all CfgStmtNode containing VarStmt
        case stmt @ CfgStmtNode(VarStmt(_, _)) => Some(stmt)
        case _ => None
      }
      .flatMap(stmt => { //zip declarations with their source CfgNode
        stmt.ast.asInstanceOf[VarStmt].decls.map(decl => (decl, stmt))
      })
      .find(x => isDeclUnused(x._1, fun)) //find unused declaration (can't perform more than 1 elimination per function)
      .map(x => DeleteDeclaration(x._1, x._2)) //create optimization action
      .toList
  }

  /** Returns true if IdentifierDeclaration is not used in any node of CFG */
  private def isDeclUnused(idDecl: IdentifierDecl, fun: FunctionCfg): Boolean = {
    fun.nodes
      .filter(_.isInstanceOf[CfgStmtNode]) //filter CfgStmtNode (other nodes are not used)
      .flatMap(node => node.ast.tree) //collect all sub ast nodes from CfgNode
      .forall { //if no ast contains identifier with a same name, declaration is unused
        case Identifier(name, _) if name == idDecl.name => false
        case _ => true
      }
  }
}
