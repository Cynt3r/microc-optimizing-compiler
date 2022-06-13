package microc.middleend.optimization

import microc.frontend.ast._
import microc.middleend.Parser
import microc.middleend.cfg.CfgStmtNode
import utest._

object SignFoldingTest extends TestSuite with Parser {
  val tests: Tests = Tests {
    /*
    test("Test optimizable code (nested folds)") {
      val code =
        """
          |main() {
          |  var x, y, z;
          |  x = 5;
          |  y = -4;
          |  z = (x == y) + (x > y);
          |  x = x > 0;
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 3
      actions.foreach(action => assertMatch(action){
        case ReplaceNode(
          BinaryOp(Equal, Identifier("x", _), Identifier("y", _), _),
          Number(0, _),
          CfgStmtNode(AssignStmt(Identifier("z", _), _, _))
        ) =>

        case ReplaceNode(
          BinaryOp(GreatThan, Identifier("x", _), Identifier("y", _), _),
          Number(1, _),
          CfgStmtNode(AssignStmt(Identifier("z", _), _, _))
        ) =>

        case ReplaceNode(
          BinaryOp(GreatThan, Identifier("x", _), Number(0, _), _),
          Number(1, _),
          CfgStmtNode(AssignStmt(Identifier("x", _), _, _))
        ) =>
      })
    }

    test("Test optimizable code (propagation of zero)") {
      val code =
        """
          |main() {
          |  var x;
          |  x = 0;
          |  return x;
          |}
          |""".stripMargin
      val actions = optimize(code)

      assertMatch(actions) {
        case List(ReplaceNode(
          Identifier("x", _),
          Number(0, _),
          CfgStmtNode(ReturnStmt(_, _)),
        )) =>
      }
    }

    test("Test code that can't be optimized") {
      val code =
        """
          |main() {
          |  var x, y, z;
          |  x = 5;
          |  y = 8;
          |  z = x == z;
          |  z = x > z;
          |  z = 1 > x;
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
    new SignFolding(analyses).run(cfg)
  }
  */
}
