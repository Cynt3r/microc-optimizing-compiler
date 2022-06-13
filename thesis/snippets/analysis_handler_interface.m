trait AnalysisHandlerInterface {
  def getTypes(program: Program): Types
  
  def getDeclarations(program: Program): Declarations
  
  def getSigns(cfg: ProgramCfg)(implicit declarations: Declarations): Option[Signs] = None
  
  def getConstants(cfg: ProgramCfg)(implicit declarations: Declarations): Option[Constants] = None
  
  def getLiveVars(cfg: ProgramCfg)(implicit declarations: Declarations): Option[LiveVars] = None
  
  def getAvailableExps(cfg: ProgramCfg)(implicit declarations: Declarations): Option[AvailableExps] = None
}