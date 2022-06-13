package microc.backend

import microc.backend.x86.X86IRCompiler
import microc.backend.x86.helpers.Instruction
import microc.backend.x86IR.{CfgCompiler, ProgramX86IR}
import microc.frontend.Frontend
import microc.frontend.ast.{FunBlockStmt, FunDecl, Loc, Number, OutputStmt, Program, ReturnStmt}
import microc.middleend.cfg.{BasicContext, BranchContext, CfgFunEntryNode, CfgFunExitNode, CfgNodeContexts, CfgStmtNode, DoWhileContext, FragmentCfg, FunctionCfg, ProgramCfg}
import microc.middleend.{AnalysesDb, Middleend}
import microc.{Language, X86Syntax}

import scala.collection.mutable

trait BackendHelper {
  protected def basicProgram: String =
    """
      |main() {
      |  var x, y, z;
      |  x = 3;
      |  y = 1 + (2 + x);
      |  z = y * 8;
      |  return z;
      |}
      |""".stripMargin

  protected def ifProgram: String =
    """
      |main() {
      |  var x;
      |  if (1) {
      |    x = 42;
      |  } else {
      |    x = -42;
      |  }
      |  return x;
      |}
      |""".stripMargin

  protected def whileProgram: String =
    """
      |main() {
      |  var x;
      |  while (1) {
      |    x = 42;
      |  }
      |  return x;
      |}
      |""".stripMargin

  protected def funProgram: String =
    """
      |foo(a, b) {
      |  var c;
      |  c = a + b;
      |  return c;
      |}
      |
      |main() {
      |  var x;
      |  x = foo(19, 23);
      |  return x;
      |}
      |""".stripMargin

  protected def recProgram: String =
    """
      |main() {
      |  var x, y;
      |  x = {a: {c: 8, d: 9}, b: 5};
      |  x.b = 42;
      |  y = x.a;
      |  return 0;
      |}
      |""".stripMargin

  protected def funRecProgram: String =
    """
      |foo(rec) {
      |  return {a: rec.b, b: rec.a};
      |}
      |
      |main() {
      |  var x;
      |  x = {a: 1, b: 2};
      |  x = foo(x);
      |  return 0;
      |}
      |""".stripMargin

  protected def pointProgram: String =
    """
      |main() {
      |  var x, y;
      |  x = 5;
      |  y = &x;
      |  *y = 8;
      |  x = *y;
      |  y = null;
      |  return 0;
      |}
      |""".stripMargin

  protected def pointRecProgram: String =
    """
      |main() {
      |  var x, y;
      |  x = {a: 5};
      |  y = &x;
      |  (*y).a = 8;
      |  return (*y).a;
      |}
      |""".stripMargin

  protected def arrProgram: String =
    """
      |main() {
      |  var x, y;
      |  x = [1, 2, 3];
      |  y = x;
      |  x[2] = 42;
      |  return y[4 / 2];
      |}
      |""".stripMargin

  protected def pointArrProgram: String =
    """
      |main() {
      |  var x, y;
      |  x = [1, 2, 3];
      |  y = &x;
      |  (*y)[0] = 42;
      |  return (*y)[0];
      |}
      |""".stripMargin

  protected def doWhileCfg(): ProgramCfg = {
    val funDecl = FunDecl(
      "main",
      List(),
      FunBlockStmt(List(), List(), ReturnStmt(Number(0, Loc(0, 0)), Loc(0,0)), Loc(0, 0)),
      Loc(0, 0)
    )
    val ast = Program(List(funDecl), Loc(0, 0))
    val entry = CfgFunEntryNode(funDecl)
    val node1 = CfgStmtNode(OutputStmt(Number(1, Loc(0, 0)), Loc(0, 0)))
    entry.succ += node1
    node1.pred += entry
    val node2 = CfgStmtNode(Number(0, Loc(0, 0)))
    node1.succ += node2
    node2.pred += node1
    node2.succ += node1
    node1.pred += node2
    val node3 = CfgStmtNode(ReturnStmt(Number(3, Loc(0, 0)), Loc(0, 0)))
    node2.succ += node3
    node3.pred += node2
    val exit = CfgFunExitNode(funDecl)
    node3.succ += exit
    exit.pred += node3
    implicit val contexts: CfgNodeContexts = mutable.Map(
      node1 -> DoWhileContext(Some(BranchContext(node1, Set(node1))), node2),
      node2 -> BasicContext,
      node3 -> BasicContext,
    )
    val fragment = new FragmentCfg(Set(entry), Set(exit))

    new ProgramCfg(ast, Set(new FunctionCfg(funDecl, entry, exit, fragment)))
  }
  /*
  protected def codeToCfg(code: String): (ProgramCfg, AnalysesDb) = {
    val program = Frontend.parse(code)
    val middleend = new Middleend(ExampleAnalysisHandler, Language.microC)
    middleend.astToCfg(program)
  }


  protected def codeToIr(code: String): ProgramX86IR = {
    val (cfg, analyses) = codeToCfg(code)
    new CfgCompiler(cfg, analyses.types, analyses.declarations).compile()
  }

  protected def codeToX86(code: String): List[Instruction] = {
    val ir = codeToIr(code)
    new X86IRCompiler(ir, X86Syntax.NASM, optimize = false).compileToInstructions()
  }
  */
}
