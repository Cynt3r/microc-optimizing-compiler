package microc.frontend

import microc.frontend.ast.Program
import microc.frontend.parser.PCParser

/** Frontend of the microC compiler that parses microC code into microC AST */
object Frontend {
  private val parser = new PCParser

  /**
   * Parses the input microC string into AST
   * @param code microC code
   * @return microC AST
   */
  def parse(code: String): Program = parser.parseProgram(code)
}
