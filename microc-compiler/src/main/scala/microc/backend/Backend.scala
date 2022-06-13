package microc.backend

import microc.X86Syntax.X86Syntax
import microc.backend.x86.X86IRCompiler
import microc.backend.x86IR.CfgCompiler
import microc.middleend.AnalysesDb
import microc.middleend.cfg.ProgramCfg

/**
 * Interface defining a backend of microC compiler
 * @tparam O type of the target output language
 */
trait Backend[+O] {

  /**
   * Compiles the input CFG IR into target language
   * @param cfg CFG IR of the program
   * @param analyses analyses of the CFG
   * @param optimize determines whether the backend should perform optimizations
   * @return target language
   */
  def compile(cfg: ProgramCfg, analyses: AnalysesDb, optimize: Boolean): O
}

/**
 * Backend of the microC compiler that compiles CFG IR into x86 assembly
 * @param syntax syntax of the produced x86 assembly (e.g., NASM)
 */
class X86Backend(syntax: X86Syntax) extends Backend[String] {

  def compile(cfg: ProgramCfg, analyses: AnalysesDb, optimize: Boolean): String = {
    val x86IR = new CfgCompiler(cfg, analyses.types, analyses.declarations).compile()
    new X86IRCompiler(x86IR, syntax, optimize).compile()
  }
}
