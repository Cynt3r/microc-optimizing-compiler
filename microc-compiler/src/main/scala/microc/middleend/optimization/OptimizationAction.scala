package microc.middleend.optimization

import microc.frontend.ast.{AstNode, Identifier, IdentifierDecl, Type}
import microc.middleend.cfg.{CfgNodeContext, CfgStmtNode}

/** Represents optimization action (delete, replace or disconnect) */
sealed trait OptimizationAction

/**
 * Represents deletion of CfgStmtNode; every successor and predecessor of the node gets reconnected to each other
 * Modifications performed:
 *  - CFG - node is removed from CFG and every successor and predecessor of the node gets reconnected to each other
 *  - contexts - context of the node is deleted
 * @param stmt deleted node
 */
case class DeleteNode(stmt: CfgStmtNode) extends OptimizationAction

/**
 * Represents replace of all occurrences of ASTNode with another ASTNode in CfgStmtNode
 * Modifications performed:
 *  - CFG - ASTNode of the node gets replaced
 * @param search ASTNode that will be replaced
 * @param replace ASTNode that will replace the searched ASTNode
 * @param subject node where the ASTNode will be replaced
 */
case class ReplaceNode(search: AstNode, replace: AstNode, subject: CfgStmtNode) extends OptimizationAction

/**
 * Represents connection of two CfgStmtNode in direction from src to dest
 * Modifications performed:
 *  - CFG - two nodes are connected to each other in one direction
 * @param src source node of the new edge in CFG
 * @param dest destination node of the new edge in CFG
 */
case class ConnectNodes(src: CfgStmtNode, dest: CfgStmtNode) extends OptimizationAction

/**
 * Represents disconnection of CfgStmtNode from CFG where no reconnecting occurs (as it does in case of DeleteNode)
 * Modifications performed:
 *  - CFG - node is disconnected from CFG
 *  - contexts - context of the node is deleted (if keepContexts is set to true)
 * @param stmt disconnected node
 * @param keepContexts if true, no changes to contexts will happen
 */
case class DisconnectNode(stmt: CfgStmtNode, keepContexts: Boolean) extends OptimizationAction

/**
 * Represents addition of a new identifier declaration
 * Modifications performed:
 *  - CFG - new declaration is added to the VarStmt
 *  - types - new pair of a declaration and its type is added to the result of the type analysis
 *  - declarations - new pair of a identifier and declaration is added to the result of the declaration analysis for each usage of the declaration
 * @param idDecl added identifier declaration
 * @param tp type of the variable
 * @param usages all usages of the new declaration
 * @param stmt node that contains VarStmt
 */
case class AddDeclaration(idDecl: IdentifierDecl, tp: Type, usages: List[Identifier], stmt: CfgStmtNode) extends OptimizationAction

/** Represents deletion of a identifier declaration */

/**
 * Represents deletion of a identifier declaration
 * Modifications performed:
 *  - CFG - declaration is removed from the VarStmt
 *  - types - type of the declaration is removed from the result of the type analysis
 *  - declarations - all occurrences of the declaration are removed from the result of the declaration analysis
 * @param idDecl removed identifier declaration
 * @param stmt node that contains VarStmt
 */
case class DeleteDeclaration(idDecl: IdentifierDecl, stmt: CfgStmtNode) extends OptimizationAction

/**
 * Represents prepending of CfgStmtNode to some other CfgStmtNode (location)
 * Modifications performed:
 *  - CFG - new node is added
 *  - contexts - context of the new node is added
 * @param stmt node added
 * @param context context of the new node
 * @param location node that the new one gets prepended to
 */
case class PrependNode(stmt: CfgStmtNode, context: CfgNodeContext, location: CfgStmtNode) extends OptimizationAction

/**
 * Represents change of a context of CfgStmtNode
 * Modifications performed:
 *  - contexts - context of the node is changed
 * @param context new context
 * @param node target node
 */
case class ChangeContext(context: CfgNodeContext, node: CfgStmtNode) extends OptimizationAction
