package microc.backend.x86IR

import microc.middleend.cfg.CfgNode

/** Builder of a x86 IR */
class X86IRBuilder {
  private var functions: List[Function] = List()
  private var funAllocs: Map[String, FunAlloc] = Map()
  private var currFun: Option[X86FunctionBuilder] = None
  private var currBb: Option[X86BbBuilder] = None
  private var freshCnt: Int = 0
  private var usedLabels: Set[String] = Set()

  /** Builds x86IR program */
  def build(): ProgramX86IR = ProgramX86IR(functions)

  /** Adds instruction to current basic block */
  def addInstruction(instr: Instruction, cfgNode: CfgNode): Instruction = {
    if (currBb.isEmpty) {
      throw new RuntimeException("Adding instruction with no active basic block")
    }
    currBb.foreach(bb => bb.addInstr(instr, cfgNode))
    instr
  }

  /** Creates a new basic block builder with same suffix (if there's no collision) as previous basic block builder */
  def createBbSameSuffix(name: String = "bb"): X86BbBuilder = createBb(sameLabel(name, forceSuffix = true))

  /** Creates a new basic block builder with a new unique suffix */
  def createBbNewSuffix(name: String = "bb"): X86BbBuilder = createBb(newLabel(name))

  /** Enters basic block in current function */
  def enterBb(bb: X86BbBuilder): X86BbBuilder = {
    if (currFun.isEmpty) {
      throw new RuntimeException("Entering basic block with no active function")
    }
    //if there's already an active basic block, leave it
    leaveBb()
    currBb = Some(bb)
    bb
  }

  /** Registers a new variable in current function */
  def addVariable(addr: Alloc): Unit = {
    if (currFun.isEmpty) {
      throw new RuntimeException("Adding variable with no active function")
    }
    currFun.get.addVariable(addr)
  }

  /** Returns instruction representing the variable from current function */
  def getIdentifier(name: String): Instruction = {
    if (currFun.isEmpty) {
      throw new RuntimeException("Accessing variable with no active function")
    }
    //first search function allocs, then variables of the current function
    funAllocs.getOrElse(name, currFun.get.getVariable(name))
  }

  /** Enters a new function and registers it */
  def enterFunction(addr: FunAlloc): Unit = {
    val lbl = sameLabel(addr.decl.name, forceSuffix = false)
    funAllocs = funAllocs + (lbl -> addr)
    currFun = Some(new X86FunctionBuilder(lbl, addr.decl.params.size))
    val entryBb = createBbNewSuffix()
    enterBb(entryBb)
  }

  /** Leaves current function */
  def leaveFunction(): Unit = {
    if (currFun.isEmpty) {
      throw new RuntimeException("Leaving function with no active function")
    }
    //if there's active basic block, leave it
    leaveBb()
    //build function and add it to list of program's functions
    val fun = currFun.get.build
    functions = functions :+ fun
    currFun = None
  }

  /** Creates a new basic block builder */
  private def createBb(name: String): X86BbBuilder = new X86BbBuilder(name)

  /** Adds basic block to the function and leaves it (if there's an active basic block) */
  private def leaveBb(): Unit = {
    if (currBb.nonEmpty) {
      val bb = currBb.get.build
      //since this method is private, we can guarantee that at this point there's always an active function
      currFun.get.addBb(bb)
      currBb = None
    }
  }

  /** Returns label with same suffix as previous label */
  private def sameLabel(name: String, forceSuffix: Boolean): String = {
    val lbl = if (forceSuffix) name + "_" + freshCnt else name
    if (usedLabels.contains(lbl)) {
      //name collision, generate new suffix
      newLabel(name)
    } else {
      usedLabels = usedLabels + lbl
      lbl
    }
  }

  /** Returns label with a new unique suffix */
  private def newLabel(name: String): String = {
    freshCnt += 1
    sameLabel(name, forceSuffix = true)
  }
}

/** Builder of a x86 IR function */
class X86FunctionBuilder(name: String, argCnt: Int) {
  private var bbs: List[BasicBlock] = List()
  private var variables: Map[String, Alloc] = Map()

  /** Builds function */
  def build: Function = Function(name, argCnt, bbs)

  /** Adds a new basic block to the function */
  def addBb(bb: BasicBlock): Unit = bbs = bbs :+ bb

  /** Returns true if function contains the basic block */
  def containsBb(bb: BasicBlock): Boolean = bbs.contains(bb)

  /** Returns all basic blocks of the function */
  def getBbs: List[BasicBlock] = bbs

  /** Registers a new variable in the function */
  def addVariable(addr: Alloc): Unit = variables = variables + (addr.decl.name -> addr)

  /** Returns an instruction representing the variable */
  def getVariable(varName: String): Alloc = {
    variables.getOrElse(varName, throw new RuntimeException(s"Variable '$varName' not found in function '$name'"))
  }
}

/** Builder of a x86 IR basic block */
class X86BbBuilder(val name: String) {
  private var instructions: List[Instruction] = List()
  private var sourceCfgNodes: Map[Instruction, CfgNode] = Map()
  private var terminated: Boolean = false

  /** Builds basic block */
  def build: BasicBlock = {
    if (!terminated) {
      throw new RuntimeException("Building non-terminated basic block")
    }
    BasicBlock(name, instructions, sourceCfgNodes)
  }

  /** Adds instruction to the basic block */
  def addInstr(instr: Instruction, cfgNode: CfgNode): Unit = {
    if (terminated) {
      throw new RuntimeException("Adding instruction to terminated basic block")
    }
    if (instr.isInstanceOf[Terminator]) {
      terminated = true
    }
    instructions = instructions :+ instr
    sourceCfgNodes = sourceCfgNodes + (instr -> cfgNode)
  }

  /** Returns instructions of the basic block */
  def getInstructions: List[Instruction] = instructions
}
