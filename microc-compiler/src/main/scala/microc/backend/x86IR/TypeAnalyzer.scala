package microc.backend.x86IR

import microc.frontend.ast
import microc.frontend.ast.{AbsentFieldType, ArrayAccess, ArrayType, BinaryOp, CallFuncExpr, Decl, Deref, Expr, FieldAccess, FunType, Identifier, Input, IntType, Null, Number, PointerType, Record, RecordType, RecursiveType, Type, UniversalType, VarRef}
import microc.middleend.analysis.{Declarations, Types}
import microc.middleend.solver.Constructor

/** Analyzer class that's used to calculate size of a type, offset of a record field, etc. */
class TypeAnalyzer(types: Types, decls: Declarations) {

  /** Returns type of a identifier declaration */
  def declIRType(id: Decl): IRType = convertType(types(id))

  /** Returns type of a return type of a function */
  def funRetIRType(expr: Expr): IRType = {
    val funTp = exprToType(expr).asInstanceOf[FunType]
    convertType(funTp.ret.asInstanceOf[Type])
  }

  /** Returns IRType and offset of a record's field */
  def fieldInfo(record: Expr, fieldName: String): (IRType, Int) = {
    val recordType = exprToType(record) match {
      case rt: RecordType => rt
      case _ => throw new RuntimeException(s"Internal error: couldn't derive the type of record: $record")
    }

    val fieldIRTypes = recordType.names
      .zip(recordType.args) //zip field names and their Type
      .map(field => (field._1, convertType(field._2.asInstanceOf[Type]))) //convert each Type to IRType
    val offset = fieldIRTypes
      .takeWhile(_._1 != fieldName) //take all fields before the fieldName
      .foldLeft(0)((acc, field) => acc + field._2.size) //sum of all their sizes will be the offset of the fieldName
    val irType = fieldIRTypes.find(_._1 == fieldName).get._2
    (irType, offset)
  }

  /** Returns IRType of the array inner element */
  def arrayElemTp(array: Expr): IRType = {
    val arrayType = exprToType(array) match {
      case rt: ArrayType => rt
      case _ => throw new RuntimeException(s"Internal error: couldn't derive the type of array: $array")
    }
    convertType(arrayType.term.asInstanceOf[Type])
  }

  /** Returns Type of an expression */
  private def exprToType(expr: Expr): Type = {
    val t = expr match {
      case _: Null => PointerType(IntType)
      case _: Number => IntType
      case id: Identifier => types(decls(id))
      case Deref(e, _) => exprToType(e).asInstanceOf[PointerType].to.asInstanceOf[Type]
      case _: BinaryOp => IntType
      case _: Input => IntType
      case VarRef(id, _) => PointerType(exprToType(id).asInstanceOf[Constructor[Type]])
      case ast.Alloc(e, _) => PointerType(exprToType(e).asInstanceOf[Constructor[Type]])

      case Record(fields, _) =>
        val names = fields.map(_.name)
        val args = fields.map(a => exprToType(a.expr).asInstanceOf[Constructor[Type]])
        RecordType(names, args)

      case FieldAccess(record, field, _) =>
        val recordType = exprToType(record).asInstanceOf[RecordType]
        recordType.names
          .zip(recordType.args) //zip field names and types
          .find(x => x._1 == field) //find correct field
          .get
          ._2 //retrieve type of the field
          .asInstanceOf[Type]

      case ast.Array(elems, _) =>
        val term = elems match {
          case e :: _ => e.asInstanceOf[Constructor[Type]]
          case Nil => IntType
        }
        val len = elems.length
        ArrayType(term, len)

      case ArrayAccess(array, _, _) =>
        exprToType(array).asInstanceOf[ArrayType].term.asInstanceOf[Type]

      case CallFuncExpr(expr, _, _) =>
        val tp = exprToType(expr).asInstanceOf[FunType]
        tp.ret.asInstanceOf[Type]
    }
    //"unwrap" recursive types
    t match {
      case RecursiveType(_, innerT) => innerT.asInstanceOf[Type]
      case _ => t
    }
  }

  /** Converts Type to IRType */
  private def convertType(t: Type): IRType = t match {
    case IntType => SimpleType
    case PointerType(to) => PointType(convertType(to.asInstanceOf[Type]))
    case AbsentFieldType => VoidType
    case UniversalType(_) => SimpleType //this happens when variable is never used, therefore no type can be inferred
    case FunType(_, _) => PointType(SimpleType)
    case RecursiveType(_, innerT) => convertType(innerT.asInstanceOf[Type])
    case RecordType(_, args) =>
      val size = args.foldLeft(0)((sum, a) => sum + convertType(a.asInstanceOf[Type]).size)
      ComposedType(size)
    case ArrayType(e, length) =>
      val size = convertType(e.asInstanceOf[Type]).size * length
      ComposedType(size)
    case _ => throw new RuntimeException("Unexpected type, can't convert to IR type")
  }
}
