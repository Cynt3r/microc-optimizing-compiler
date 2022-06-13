class Compiler(analysisHandler: AnalysisHandlerInterface, lang: Language, x86Syntax: X86Syntax) {
    def compile(code: String, optimize: Boolean = false): String = {...}
}