package microc

/** Example of usage of the microC compiler */
object MainClass {

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      throw new RuntimeException("Missing a filename of the microC source code as the first argument")
    }
    val buf = scala.io.Source.fromFile(args(0))
    val code = try buf.mkString finally buf.close()
    val optimize = args.length >= 2 && args(1) == "-o"
    //TODO: provide your implementation of `AnalysisHandlerInterface`
    val compiler = new Compiler(???, Language.microC, X86Syntax.NASM)
    val assembly = compiler.compile(code, optimize)
    println(assembly)
  }
}
