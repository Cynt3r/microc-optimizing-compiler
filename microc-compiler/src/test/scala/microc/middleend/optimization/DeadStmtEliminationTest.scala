package microc.middleend.optimization

import microc.frontend.ast.{AssignStmt, BinaryOp, DirectFieldWrite, Identifier, Input, Times}
import microc.middleend.Parser
import microc.middleend.cfg.CfgStmtNode
import utest._

object DeadStmtEliminationTest extends TestSuite with Parser {
  val tests: Tests = Tests {
    /*
    test("Test optimizable code (simple assigns)") {
      val code =
        """
          |main() {
          |  var x, y, z;
          |  x = 5;
          |  y = 4;
          |  z = y;
          |  output x;
          |  return y;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 1
      assertMatch(actions.head){case DeleteNode(CfgStmtNode(AssignStmt(Identifier("z", _), _, _))) =>}
    }

    test("Test optimizable code (record assigns)") {
      val code =
        """
          |main() {
          |  var x;
          |  x = {a: 8};
          |  output x.a;
          |  x.a = 10;
          |  x = {a: 0};
          |  return x.a;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 1
      assertMatch(actions.head){case DeleteNode(CfgStmtNode(AssignStmt(DirectFieldWrite(Identifier("x", _), "a", _), _, _))) =>}
    }

    test("Test optimizable code (assign with side-effect)") {
      val code =
        """
          |main() {
          |  var x, y;
          |  x = 5;
          |  y = input;
          |  return x;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 1
      assertMatch(actions.head){
        case ReplaceNode(
          AssignStmt(Identifier("y", _), Input(_), _),
          Input(_),
          CfgStmtNode(AssignStmt(Identifier("y", _), Input(_), _)),
        ) =>
      }
    }

    test("Test optimizable code (assign with nested side-effect)") {
      val code =
        """
          |main() {
          |  var x;
          |  x = 5 * (2 + input);
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 1
      assertMatch(actions.head){
        case ReplaceNode(
          AssignStmt(Identifier("x", _), BinaryOp(Times, _, _, _), _),
          BinaryOp(Times, _, _, _),
          CfgStmtNode(AssignStmt(Identifier("x", _), BinaryOp(Times, _, _, _), _)),
        ) =>
      }
    }

    test("Test code that can't be optimized (chained dependency)") {
      val code =
        """
          |main() {
          |  var x, y, z;
          |  x = 5;
          |  y = 4;
          |  z = y;
          |  output x;
          |  return z;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 0
    }

    test("Test code that can't be optimized (assign to record)") {
      val code =
        """
          |main() {
          |  var x, y;
          |  x = 5;
          |  y = {a: x, b: x};
          |  y.a = 8;
          |  return y.a;
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
    new DeadStmtElimination(analyses).run(cfg)
  }
  */
}
