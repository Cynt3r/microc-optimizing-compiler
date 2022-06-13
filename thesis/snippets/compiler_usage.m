val code = ... //the input microC program
val compiler = new Compiler(MyAnalysisHandler, Language.microCVar, X86Syntax.NASM)
val assembly = compiler.compile(code, optimize = true)