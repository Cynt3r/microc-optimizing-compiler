case class Function(
    name: String,
    argCnt: Int,
    bbs: List[BasicBlock]
) {
  def getLocals: List[Alloc] = {...}
  def getArgs: List[Alloc] = {...}
}