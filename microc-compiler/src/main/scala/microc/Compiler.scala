package microc

import microc.Language.Language
import microc.X86Syntax.X86Syntax
import microc.backend.X86Backend
import microc.frontend.Frontend
import microc.middleend.Middleend
import microc.middleend.analysis.AnalysisHandlerInterface

/** Enum of supported x86 syntaxes */
object X86Syntax extends Enumeration {
  type X86Syntax = Value

  /** NASM syntax */
  val NASM: X86Syntax.Value = Value
}

/** Enum of supported microC language subsets */
object Language extends Enumeration {
  type Language = Value

  /** Basic microC constructs with no control flow */
  val microCVar: Language.Value = Value(1)
  /** MicroCVar with if/else statements */
  val microCIf: Language.Value = Value(2)
  /** MicroCIf with while statements */
  val microCWhile: Language.Value = Value(3)
  /** MicroCWhile with function calls */
  val microCFun: Language.Value = Value(4)
  /** MicroCFun with records */
  val microCRec: Language.Value = Value(5)
  /** MicroCRec with arrays */
  val microCArr: Language.Value = Value(6)
  /** Entire microC langauge */
  val microC: Language.Value = Value(7)
}

/**
 * Compiler for microC that produces x86 assembly
 * @param analysisHandler interface that defines methods for retrieving results of analyses
 * @param lang defines language of the compiler; compiling code that contains constructs not present in the language will result in exception
 * @param x86Syntax syntax of the output assembly
 */
class Compiler(analysisHandler: AnalysisHandlerInterface, lang: Language, x86Syntax: X86Syntax) {

  /**
   * Compiles given code and returns x86 assembly as a string
   * @param code code to be compiled
   * @param optimize if true, compiler will perform optimizations
   * @return x86 assembly
   */
  def compile(code: String, optimize: Boolean = false): String = {
    val program = Frontend.parse(code) //string -> AST
    val middleend = new Middleend(analysisHandler, lang)
    val (cfg, analyses) = middleend.astToCfg(program, optimize) //AST -> CFG
    val backend = new X86Backend(x86Syntax)
    backend.compile(cfg, analyses, optimize) //CFG -> x86
  }
}
