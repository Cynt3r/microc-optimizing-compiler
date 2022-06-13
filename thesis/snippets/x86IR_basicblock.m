case class BasicBlock(
    name: String,
    instructions: List[Instruction],
    sourceCfgNodes: Map[Instruction, CfgNode]
)