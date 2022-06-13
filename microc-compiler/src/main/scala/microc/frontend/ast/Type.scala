package microc.frontend.ast

import microc.middleend.solver._

sealed trait Type

/**
  * The int type.
  */
case object IntType extends Type with Constructor[Type] {
  val args: List[Term[Type]] = Nil

  def subst(v: Var[Type], t: Term[Type]): Constructor[Type] = this

  override def toString: String = "int"
}

/**
  * Pointer type.
  *
  * @param to the pointer target
  */
case class PointerType(to: Term[Type]) extends Type with Constructor[Type] {
  val args: List[Term[Type]] = List(to)

  def subst(v: Var[Type], t: Term[Type]): Constructor[Type] = PointerType(to.subst(v, t))

  override def toString: String = s"↑$to"
}

/**
  * Function type.
  *
  * @param params the function parameters
  * @param ret the return type
  */
case class FunType(params: List[Term[Type]], ret: Term[Type]) extends Type with Constructor[Type] {
  override val args: List[Term[Type]] = ret :: params

  override def subst(v: Var[Type], t: Term[Type]): Constructor[Type] = FunType(params.map(p => p.subst(v, t)), ret.subst(v, t))

  override def toString: String = s"(${params.mkString(",")}) -> $ret"
}

/**
  * Type variable for a program variable or expression.
  *
  * @param node the AST node associated with this type variable
  */
case class VarType(node: AstNode) extends Type with Var[Type] {
  require(!node.isInstanceOf[Identifier], "Cannot make a type variable for an identifier")

  override def toString: String = s"⟦$node⟧"
}

case class UniversalType(id: Int) extends Type with Var[Type] {
  override def toString: String = s"t$id"
}

/**
  * Recursive type of the form: µv.t.
  *
  * It is only ever while closing other terms.
  *
  * For example: µt.(↑int,t) -> int
  *
  *  which would expand to:
  *
  *  (↑int,(↑int,(↑int,...) -> int) -> int) -> int
  *
  * @param variable the type variable
  * @param term the recursive type term the type variable represents
  *
  */
case class RecursiveType(variable: Var[Type], term: Term[Type]) extends Type with Mu[Type] {
  def subst(sv: Var[Type], to: Term[Type]): Term[Type] =
    if (sv == variable) this else RecursiveType(variable, term.subst(sv, to))
}

/**
  * Record type.
  *
  * A record type is represented as a term with a sub-term for every field name in the entire program.
  */
case class RecordType(names: List[String], args: List[Term[Type]]) extends Type with Constructor[Type] {
  def subst(v: Var[Type], t: Term[Type]): Constructor[Type] =
    RecordType(names, args.map { p =>
      p.subst(v, t)
    })

  override def toString: String =
    names.zip(args).map { case (n, t) => s"$n:$t" }.mkString("{", ",", "}")
}

case class ArrayType(term: Term[Type], length: Int) extends Type with Constructor[Type] {
  override val args: List[Term[Type]] = List(term)

  override def subst(v: Var[Type], t: Term[Type]): Term[Type] = ArrayType(term.subst(v, t), length)

  override def toString: String = term.toString + "[]"
}

case object AbsentFieldType extends Type with Constructor[Type] {
  val args: List[Term[Type]] = Nil

  def subst(v: Var[Type], t: Term[Type]): Constructor[Type] = this

  override def toString: String = "◇"
}
