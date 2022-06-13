/**
 * Compiler for microC that produces x86 assembly
 * @param analysisHandler interface that defines methods ...
 * @param lang defines language of the compiler
 * @param x86Syntax syntax of the output assembly
 */
class Compiler(analysisHandler: AnalysisHandlerInterface, lang: Language, x86Syntax: X86Syntax) {

    /**
     * Compiles given code and returns x86 assembly as a string
     * @param code code to be compiled
     * @param optimize if true,compiler will perform optimizations
     * @return x86 assembly
     */
    def compile(code: String, optimize: Boolean = false): String
}