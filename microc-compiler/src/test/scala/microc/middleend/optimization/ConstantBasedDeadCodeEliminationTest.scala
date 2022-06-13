package microc.middleend.optimization

import microc.frontend.ast.{AssignStmt, BinaryOp, GreatThan, Identifier, Number, OutputStmt, ReturnStmt}
import microc.middleend.Parser
import microc.middleend.cfg.{BasicContext, BranchContext, CfgStmtNode, DoWhileContext}
import utest._

object ConstantBasedDeadCodeEliminationTest extends TestSuite with Parser {
  val tests: Tests = Tests {
    /*
    test("Test optimizable code (while to do-while)") {
      val code =
        """
          |main() {
          |  var x;
          |  x = 5;
          |  while (x) {
          |    output x;
          |    x = x - 1;
          |  }
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 7
      actions.foreach(action => assertMatch(action){
        case DisconnectNode(CfgStmtNode(Identifier("x", _)), true) =>

        case PrependNode(
          CfgStmtNode(Number(0, _)),
          DoWhileContext(
            Some(BranchContext(CfgStmtNode(Number(0, _)), _)),
            CfgStmtNode(Identifier("x", _)),
          ),
          CfgStmtNode(OutputStmt(_, _)),
        ) =>

        case ConnectNodes(
          CfgStmtNode(AssignStmt(_, Number(5, _), _)),
          CfgStmtNode(Number(0, _)),
        ) =>

        case ConnectNodes(
          CfgStmtNode(AssignStmt(_, BinaryOp(_, _, _, _), _)),
          CfgStmtNode(Identifier("x", _)),
        ) =>

        case ConnectNodes(
          CfgStmtNode(Identifier("x", _)),
          CfgStmtNode(Number(0, _)),
        ) =>

        case ConnectNodes(
          CfgStmtNode(Identifier("x", _)),
          CfgStmtNode(ReturnStmt(_, _)),
        ) =>

        case ChangeContext(
          BasicContext,
          CfgStmtNode(Identifier("x", _)),
        ) =>
      })
    }

    test("Test optimizable code (while loop elimination)") {
      val code =
        """
          |main() {
          |  var x;
          |  x = 0;
          |  while (x > 0) {
          |    output x;
          |    x = x + 1;
          |  }
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 3
      actions.foreach(action => assertMatch(action){
        case DeleteNode(CfgStmtNode(BinaryOp(GreatThan, _, _, _))) =>
        case DisconnectNode(CfgStmtNode(OutputStmt(_, _)), false) =>
        case DisconnectNode(CfgStmtNode(AssignStmt(_, BinaryOp(_, _, _, _), _)), false) =>
      })
    }

    test("Test optimizable code (if-branch elimination)") {
      val code =
        """
          |main() {
          |  var x;
          |  if (input) {
          |    x = 5;
          |  } else {
          |    x = 5;
          |  }
          |  if (x) {
          |    output 1;
          |  } else {
          |    output 0;
          |  }
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 2
      actions.foreach(action => assertMatch(action){
        case DeleteNode(CfgStmtNode(Identifier("x", _))) =>
        case DisconnectNode(CfgStmtNode(OutputStmt(Number(0, _), _)), false) =>
      })
    }

    test("Test code that can't be optimized") {
      val code =
        """
          |main() {
          |  var x;
          |  x = input;
          |  while (x) {
          |    output x;
          |    x = 0;
          |  }
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 0
    }
    */
  }

  /*
  private def optimize(code: String): List[OptimizationAction] = {
    val (cfg, analyses) = parseCode(code)
    new ConstantBasedDeadCodeElimination(analyses).run(cfg)
  }
  */
}
