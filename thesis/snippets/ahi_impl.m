object MyAnalysisHandler extends AnalysisHandlerInterface {
    override def getTypes(program: Program): Types = {
        //your implementation of a type analysis (required)
        new TypeAnalysis(program, NoopLogger)(getDeclarations(program)).analyze()
    }
    override def getDeclarations(program: Program): Declarations = {
        //your implementation of a declaration analysis (required)
        new DeclarationAnalysis(program).analyze()
    }
    override def getSigns(cfg: ProgramCfg)(implicit declarations: Declarations): Option[Signs] = {
        //your implementation of a sign analysis
        val signs = new SignAnalysis(cfg).analyze()
        Some(signs)
    }
}