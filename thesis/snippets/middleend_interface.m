class Middleend(
    handler: AnalysisHandlerInterface,
    lang: Language = Language.microC
) {
    def astToCfg(
        program: Program,
        optimize: Boolean = false
    ): (ProgramCfg, AnalysesDb) = {...}
}