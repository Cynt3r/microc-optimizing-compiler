package microc.middleend.optimization

import microc.frontend.ast._
import microc.middleend.Parser
import microc.middleend.cfg.CfgStmtNode
import utest._

object ConstantPropagationTest extends TestSuite with Parser {
  val tests: Tests = Tests {
    /*
    test("Test optimizable code") {
      val code =
        """
          |main() {
          |  var x, y, z;
          |  x = 5;
          |  y = x * (x + x);
          |  z = 10;
          |  return x - z;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 5
      actions.foreach(action => assertMatch(action){
        case ReplaceNode(
          Identifier("x", _),
          Number(5, _),
          CfgStmtNode(AssignStmt(Identifier("y", _), _, _))
        ) =>

        case ReplaceNode(
          Identifier("x", _),
          Number(5, _),
          CfgStmtNode(ReturnStmt(_, _))
        ) =>

        case ReplaceNode(
          Identifier("z", _),
          Number(10, _),
          CfgStmtNode(ReturnStmt(_, _))
        ) =>
      })
    }

    test("Test code that can't be optimized (input)") {
      val code =
        """
          |main() {
          |  var x, y, z;
          |  x = input;
          |  y = 4 + x;
          |  z = y;
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 0
    }

    test("Test code that can't be optimized (variable reference)") {
      val code =
        """
          |main() {
          |  var x, y;
          |  x = 5;
          |  y = &x;
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
    new ConstantPropagation(analyses).run(cfg)
  }
  */
}
