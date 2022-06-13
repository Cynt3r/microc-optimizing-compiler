package microc.middleend.optimization

import microc.frontend.ast.{AssignStmt, BinaryOp, Identifier, IdentifierDecl, IntType, Number, Times, VarStmt}
import microc.middleend.Parser
import microc.middleend.cfg.{BasicContext, CfgStmtNode}
import utest._

object CommonSubexpEliminationTest extends TestSuite with Parser {
  val tests: Tests = Tests {
    /*
    test("Test optimizable code (nested expressions)") {
      val code =
        """
          |main() {
          |  var x, y, z;
          |  x = 42 + (86 * (1 + 1));
          |  if (input) {
          |    y = 86 * (1 + 1);
          |  }
          |  return x + y;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 4
      actions.foreach(action => assertMatch(action){
        case AddDeclaration(
          IdentifierDecl("t0", _),
          IntType,
          List(_, _, _), //three usages (two binOps and one assignment)
          CfgStmtNode(VarStmt(_, _))
        ) =>

        case ReplaceNode(
          BinaryOp(Times, Number(86, _), BinaryOp(_, _, _, _), _),
          Identifier("t0", _),
          CfgStmtNode(AssignStmt(Identifier("x", _), _, _)),
        ) =>

        case ReplaceNode(
          BinaryOp(Times, Number(86, _), BinaryOp(_, _, _, _), _),
          Identifier("t0", _),
          CfgStmtNode(AssignStmt(Identifier("y", _), _, _)),
        ) =>

        case PrependNode(
          CfgStmtNode(AssignStmt(Identifier("t0", _), BinaryOp(Times, Number(86, _), BinaryOp(_, _, _, _), _), _)),
          BasicContext,
          CfgStmtNode(AssignStmt(Identifier("x", _), _, _)),
        ) =>
      })
    }

    test("Test optimizable code (self-referencing assign)") {
      val code =
        """
          |main() {
          |  var x, y;
          |  x = input;
          |  y = x * 10;
          |  x = x * 10;
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 4
      actions.foreach(action => assertMatch(action){
        case AddDeclaration(
          IdentifierDecl("t0", _),
          IntType,
          List(_, _, _), //three usages (two binOps and one assignment)
          CfgStmtNode(VarStmt(_, _))
        ) =>

        case ReplaceNode(
          BinaryOp(Times, Identifier("x", _), Number(10, _), _),
          Identifier("t0", _),
          CfgStmtNode(AssignStmt(Identifier("y", _), _, _)),
        ) =>

        case ReplaceNode(
          BinaryOp(Times, Identifier("x", _), Number(10, _), _),
          Identifier("t0", _),
          CfgStmtNode(AssignStmt(Identifier("x", _), _, _)),
        ) =>

        case PrependNode(
          CfgStmtNode(AssignStmt(Identifier("t0", _), BinaryOp(Times, Identifier("x", _), Number(10, _), _), _)),
          BasicContext,
          CfgStmtNode(AssignStmt(Identifier("y", _), _, _)),
        ) =>
      })
    }

    test("Test optimizable code (commutative expressions)") {
      val code =
        """
          |main() {
          |  var x;
          |  x = 2 * 3;
          |  x = 3 * 2;
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 4
      actions.foreach(action => assertMatch(action){
        case AddDeclaration(
          IdentifierDecl("t0", _),
          IntType,
          List(_, _, _), //three usages (two binOps and one assignment)
          CfgStmtNode(VarStmt(_, _))
        ) =>

        case ReplaceNode(
          BinaryOp(Times, Number(2, _), Number(3, _), _),
          Identifier("t0", _),
          CfgStmtNode(AssignStmt(Identifier("x", _), _, _)),
        ) =>

        case ReplaceNode(
          BinaryOp(Times, Number(3, _), Number(2, _), _),
          Identifier("t0", _),
          CfgStmtNode(AssignStmt(Identifier("x", _), _, _)),
        ) =>

        case PrependNode(
          CfgStmtNode(AssignStmt(Identifier("t0", _), BinaryOp(Times, Number(2, _), Number(3, _), _), _)),
          BasicContext,
          CfgStmtNode(AssignStmt(Identifier("x", _), _, _)),
        ) =>
      })
    }

    test("Test code that can't be optimized (if-else branches)") {
      val code =
        """
          |main() {
          |  var x, y, z;
          |  if (input) {
          |    x = 5 / 4;
          |  } else {
          |    x = 5 / 4;
          |  }
          |  output 5 / 4;
          |  return 0;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 0
    }

    test("Test code that can't be optimized (expressions with side-effects)") {
      val code =
        """
          |foo() {
          |  return input;
          |}
          |
          |main() {
          |  var x, y;
          |  x = input + (5 + foo());
          |  y = input + (5 + foo());
          |  return x + y;
          |}
          |""".stripMargin
      val actions = optimize(code)

      actions.size ==> 0
    }

    test("Test code that can't be optimized (non-commutative expressions)") {
      val code =
        """
          |main() {
          |  var x;
          |  x = 2 - 1;
          |  x = 1 - 2;
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
    new CommonSubexpElimination(analyses).run(cfg)
  }
  */
}
