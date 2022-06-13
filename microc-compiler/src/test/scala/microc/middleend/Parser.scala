package microc.middleend

import microc.Language
import microc.frontend.Frontend
import microc.middleend.cfg.ProgramCfg

trait Parser {
  /*
  protected def parseCode(code: String): (ProgramCfg, AnalysesDb) = {
    val program = Frontend.parse(code)
    val middleend = new Middleend(ExampleAnalysisHandler, Language.microC)
    middleend.astToCfg(program)
  }
  */
}
