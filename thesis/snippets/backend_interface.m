trait Backend[+O] {
    def compile(
        cfg: ProgramCfg,
        analyses: AnalysesDb,
        optimize: Boolean
    ): O
}

class X86Backend(syntax: X86Syntax) extends Backend[String] {
    def compile(
        cfg: ProgramCfg,
        analyses: AnalysesDb,
        optimize: Boolean
    ): String = {...}
}