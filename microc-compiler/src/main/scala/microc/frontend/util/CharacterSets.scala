package microc.frontend.util

object CharacterSets {
  val NL = System.lineSeparator()

  def isNewLine(ch: Char): Boolean = ch == '\n' || ch == '\r'
}
