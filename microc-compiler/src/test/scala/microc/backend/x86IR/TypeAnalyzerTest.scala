package microc.backend.x86IR

import microc.backend.BackendHelper
import microc.frontend.ast.{Decl, Identifier}
import microc.middleend.AnalysesDb
import utest._

object TypeAnalyzerTest extends TestSuite with BackendHelper {
  val tests: Tests = Tests {
    /*
    test("Inference of declaration IRType") {
      val (_, analyses) = codeToCfg(program)
      val ta = new TypeAnalyzer(analyses.types, analyses.declarations)
      val x = getDecl("x", analyses)
      val y = getDecl("y", analyses)
      val z = getDecl("z", analyses)
      val f = getDecl("f", analyses)

      ta.declIRType(x) ==> ComposedType(32)
      ta.declIRType(y) ==> SimpleType
      ta.declIRType(z) ==> ComposedType(16)
      ta.declIRType(f) ==> PointType(SimpleType)
    }

    test("Inference of function return IRType") {
      val (_, analyses) = codeToCfg(program)
      val ta = new TypeAnalyzer(analyses.types, analyses.declarations)
      val f = getIdent("f", analyses)

      ta.funRetIRType(f) ==> PointType(ComposedType(8))
    }

    test("Inference of field's IRType and offset") {
      val (_, analyses) = codeToCfg(program)
      val ta = new TypeAnalyzer(analyses.types, analyses.declarations)
      val z = getIdent("z", analyses)

      ta.fieldInfo(z, "arr") ==> (PointType(ComposedType(32)), 0)
      ta.fieldInfo(z, "size") ==> (SimpleType, 8)
    }

    test("Inference of array element IRType") {
      val (_, analyses) = codeToCfg(program)
      val ta = new TypeAnalyzer(analyses.types, analyses.declarations)
      val x = getIdent("x", analyses)

      ta.arrayElemTp(x) ==> SimpleType
    }
    */
  }

  private def program: String =
    """
      |foo() {
      | return alloc [42];
      |}
      |
      |main() {
      | var x, y, z, f;
      | x = [1, 2, 3, 4];
      | y = 4;
      | z = {arr: &x, size: y};
      | f = foo;
      | return 0;
      |}
      |""".stripMargin

  private def getDecl(name: String, analyses: AnalysesDb): Decl =
    analyses.declarations.values.find(_.name == name).get

  private def getIdent(name: String, analyses: AnalysesDb): Identifier =
    analyses.declarations.keys.find(_.name == name).get
}
