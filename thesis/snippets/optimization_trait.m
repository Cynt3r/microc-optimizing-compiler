trait Optimization {
    val cost: Int
    def isRunnable: Boolean
    def run(cfg: ProgramCfg): List[OptimizationAction]
}