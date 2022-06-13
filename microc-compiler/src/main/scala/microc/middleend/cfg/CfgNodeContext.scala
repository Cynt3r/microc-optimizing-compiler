package microc.middleend.cfg

/** Represents additional context of CfgStmtNode (and its successors) */
sealed trait CfgNodeContext

/** Context for statement that doesn't introduce any control flow (assignment, return, etc.) */
case object BasicContext extends CfgNodeContext

/** Context for If/Else statement (guard is the CfgStmtNode for which this context was defined)  */
case class IfContext(thenBranch: Option[BranchContext], elseBranch: Option[BranchContext]) extends CfgNodeContext

/** Context for While statement (guard is the CfgStmtNode for which this context was defined) */
case class WhileContext(bodyBranch: Option[BranchContext]) extends CfgNodeContext

/** Context for Do-While statement (this context is defined for the first node of the body) */
case class DoWhileContext(bodyBranch: Option[BranchContext], guard: CfgNode) extends CfgNodeContext

/** Context for branch defined by single entry node and one or more exit nodes */
case class BranchContext(entry: CfgNode, exit: Set[CfgNode])
