package microc.backend.x86

import microc.backend.x86IR.{BasicBlock, Instruction, ProgramX86IR}

/** Performs live instruction analysis and gives access to the result of the analysis */
class LiveInstructionAnalysis(program: ProgramX86IR) {
  type LiveInstructions = Map[Instruction, Set[Instruction]]

  /** Maps basic block (based on its unique name) to live instructions map */
  private val liveBbInstructions: Map[String, LiveInstructions] = analyze
  /** Maps basic block (based on its unique name) to order in which instruction will be used as operands */
  private val usageOrders: Map[String, List[Instruction]] = getUsageOrder

  /** Returns instructions that will be alive after given `instr` in the basic block */
  def liveAfterInstr(bbName: String, instr: Instruction): Set[Instruction] = liveBbInstructions(bbName)(instr)

  /** Returns order in which instructions will be used as operands in the given basic block */
  def usageOrder(bbName: String): List[Instruction] = usageOrders.getOrElse(bbName, List())

  /** Analyzes the whole program and returns live instruction of each basic block */
  private def analyze: Map[String, LiveInstructions] = {
    program.functions
      .flatMap(_.bbs) //retrieve all basic blocks
      .map(bb => (bb.name, analyzeBb(bb))) //map basic blocks to their live instruction analysis
      .toMap
  }

  private def analyzeBb(bb: BasicBlock): LiveInstructions = bb.instructions match {
    case Nil => Map()
    case instructions =>
      //instruction will be analyzed one by one in reverse
      analyzeInstructions(instructions.reverse, Set())
  }

  private def analyzeInstructions(instructions: List[Instruction], live: Set[Instruction]): LiveInstructions = instructions match {
    case Nil => Map()
    case instr :: rest =>
      //add children of instr and remove instr from live instructions
      val nextLive = (live ++ instr.operands) - instr
      analyzeInstructions(rest, nextLive) + (instr -> live)
  }

  /** Retrieves orders of instruction usages */
  private def getUsageOrder: Map[String, List[Instruction]] = {
    program.functions
      .flatMap(_.bbs) //retrieve all basic blocks
      .map(bb => {
        //retrieve instructions in order they will be used as operands
        val order = bb.instructions.flatMap(_.operands).distinct
        (bb.name, order)
      })
      .toMap
  }
}
