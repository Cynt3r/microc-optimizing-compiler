package microc.middleend.optimization

import microc.frontend.ast._
import microc.middleend.Parser
import microc.middleend.cfg.CfgStmtNode
import utest._

object BasicDeadCodeEliminationTest extends TestSuite with Parser {
  val tests: Tests = Tests {
    /*
    test("Test optimizable code") {
      val code =
        """
          |main() {
          |  var x, y;
          |  if (1) {
          |    y = 1;
          |    x = 10;
          |  } else {
          |    x = 0;
          |    y = 0;
          |  }
          |  output x;
          |  return y;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 3
      actions.foreach(action => assertMatch(action){
        case DeleteNode(CfgStmtNode(Number(1, _))) =>

        case DisconnectNode(CfgStmtNode(AssignStmt(Identifier("x", _), Number(0, _), _)), false) =>

        case DisconnectNode(CfgStmtNode(AssignStmt(Identifier("y", _), Number(0, _), _)), false) =>
      })
    }

    test("Test code that can't be optimized") {
      val code =
        """
          |main() {
          |  var x, y;
          |  x = input;
          |  if (x > 0) {
          |    y = 1;
          |  } else {
          |    y = 0;
          |  }
          |  output x;
          |  return y;
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
    new BasicDeadCodeElimination().run(cfg)
  }
  */
}
