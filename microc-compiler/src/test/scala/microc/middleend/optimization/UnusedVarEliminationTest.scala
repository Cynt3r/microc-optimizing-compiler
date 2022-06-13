package microc.middleend.optimization

import microc.frontend.ast.{IdentifierDecl, Loc, VarStmt}
import microc.middleend.Parser
import microc.middleend.cfg.CfgStmtNode
import utest._

object UnusedVarEliminationTest extends TestSuite with Parser {
  val tests: Tests = Tests {
    /*
    test("Test optimizable code") {
      val code =
        """
          |foo() {
          |  var x;
          |  return 0;
          |}
          |
          |main() {
          |  var x, y;
          |  x = 5;
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 2
      actions.foreach(action => assertMatch(action){
        case DeleteDeclaration(
          IdentifierDecl("x", Loc(3, 7)),
          CfgStmtNode(VarStmt(_, _)),
        ) =>

        case DeleteDeclaration(
          IdentifierDecl("y", Loc(8, 10)),
          CfgStmtNode(VarStmt(_, _)),
        ) =>
      })
    }

    test("Test code that can't be optimized") {
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
    */
  }

  /*
  private def optimize(code: String): List[OptimizationAction] = {
    val (cfg, _) = parseCode(code)
    new UnusedVarElimination().run(cfg)
  }
  */
}
