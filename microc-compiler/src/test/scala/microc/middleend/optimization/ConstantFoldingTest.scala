package microc.middleend.optimization

import microc.frontend.ast._
import microc.middleend.Parser
import microc.middleend.cfg.CfgStmtNode
import utest._

object ConstantFoldingTest extends TestSuite with Parser {
  val tests: Tests = Tests {
    /*
    test("Test optimizable code (nested folds)") {
      val code =
        """
          |main() {
          |  var x, y, z;
          |  x = (20 * 2) + (5 + 1);
          |  y = 4 + x;
          |  z = 1 + 1;
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 3
      actions.foreach(action => assertMatch(action){
        case ReplaceNode(
          BinaryOp(Plus, Number(5, _), Number(1, _), _),
          Number(6, _),
          CfgStmtNode(AssignStmt(Identifier("x", _), _, _))
        ) =>

        case ReplaceNode(
          BinaryOp(Times, Number(20, _), Number(2, _), _),
          Number(40, _),
          CfgStmtNode(AssignStmt(Identifier("x", _), _, _))
        ) =>

        case ReplaceNode(
          BinaryOp(Plus, Number(1, _), Number(1, _), _),
          Number(2, _),
          CfgStmtNode(AssignStmt(Identifier("z", _), _, _))
        ) =>
      })
    }

    test("Test optimizable code (algebraic rules)") {
      val code =
        """
          |main() {
          |  var x, y, z;
          |  x = input;
          |  y = 1 * x;
          |  z = 0 + x;
          |  z = x / 1;
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 3
      actions.foreach(action => assertMatch(action){
        case ReplaceNode(
          BinaryOp(Times, Number(1, _), Identifier("x", _), _),
          Identifier("x", _),
          CfgStmtNode(AssignStmt(Identifier("y", _), _, _))
        ) =>

        case ReplaceNode(
          BinaryOp(Plus, Number(0, _), Identifier("x", _), _),
          Identifier("x", _),
          CfgStmtNode(AssignStmt(Identifier("z", _), _, _))
        ) =>

        case ReplaceNode(
          BinaryOp(Divide, Identifier("x", _), Number(1, _), _),
          Identifier("x", _),
          CfgStmtNode(AssignStmt(Identifier("z", _), _, _))
        ) =>
      })
    }

    test("Test code that can't be optimized (variables that can't be folded)") {
      val code =
        """
          |main() {
          |  var x, y, z;
          |  x = 5;
          |  y = 4 + x;
          |  z = input;
          |  z = 3 * (3 * z);
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 0
    }

    test("test code that can't be optimized (operations with side-effects)") {
      val code =
        """
          |main() {
          |  var x, y;
          |  x = 5;
          |  y = 0 * input;
          |  y = input * 0;
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
    val (cfg, _) = parseCode(code)
    new ConstantFolding().run(cfg)
  }
  */
}
