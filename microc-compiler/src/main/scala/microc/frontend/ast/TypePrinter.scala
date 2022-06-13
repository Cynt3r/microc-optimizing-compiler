package microc.frontend.ast

import microc.frontend.util.CharacterSets.NL
import microc.frontend.util.Collections._
import microc.frontend.util.IndentWriter
import microc.middleend.analysis.Types

import scala.annotation.tailrec

class TypePrinter(types: Types, indent: Option[Int] = Some(2), explicitFreeTypeVars: Boolean = false)
    extends AstPrinter(indent) {
  override def visit(node: AstNode, out: IndentWriter): Unit = node match {
    case d @ IdentifierDecl(name, _) =>
      out << name
      out << ": "
      out << typeOf(d)

    case d @ FunDecl(name, params, block, _) =>
      out << name
      if (explicitFreeTypeVars) {
        out << freeTypeVars(d)
      }
      out << "("
      params.foreachSep(visit(_, out), out << ",")
      out << "): " << typeOf(d) << NL
      visit(block, out)

    case x => super.visit(x, out)
  }

  private def freeTypeVars(d: FunDecl): String = {
    val vars = funType(d).unboundedVariables.map(_.toString).toList.sorted

    vars match {
      case Nil => ""
      case _   => vars.mkString("[", ",", "]")
    }
  }

  private def typeOf(d: FunDecl): String = funType(d).ret.toString

  private def typeOf(d: Decl): String = types.get(d).map(_.toString).getOrElse("??")

  private def funType(d: FunDecl): FunType = {
    @tailrec def inner(t: Type): FunType = t match {
      case x: FunType          => x
      case RecursiveType(_, y) => inner(y.asInstanceOf[Type])
      case x                   => throw new IllegalStateException(s"Expected FunType for $d, got: $x")
    }

    inner(types.getOrElse(d, throw new IllegalStateException(s"Missing FunType for $d")))
  }
}
