package microc.backend.x86

import microc.X86Syntax
import microc.X86Syntax.X86Syntax
import microc.backend.x86.helpers._

/** Builder of an x86 program */
class X86Builder() {
  private var instructions: List[Instruction] = List()
  /** Maps function names to their memory location name */
  private var funLocations: Map[String, String] = Map()

  /** Adds instruction to the builder and returns its id */
  def add(instr: Instruction): Int = {
    val id = instructions.size
    instructions = instructions :+ instr
    id
  }

  /** Replaces instruction with another instruction based on the given id */
  def patch(id: Int, instr: Instruction): Unit = {
    instructions = instructions.updated(id, instr)
  }

  /** Registers function and its location in memory */
  def registerFun(name: String, memLocation: String): Unit = {
    funLocations = funLocations + (name -> memLocation)
  }

  /** Returns memory location of the function */
  def funMemLocation(name: String): String = funLocations(name)

  /** Returns all instructions */
  def getInstructions: List[Instruction] = instructions

  /** Builds and serializes x86 instructions into a string */
  def build(syntax: X86Syntax, optimize: Boolean): String = {
    //perform peephole optimizations if true
    if (optimize) {
      instructions = Peepholer.optimize(instructions)
    }

    //get proper formatter
    val formatter = syntax match {
      case X86Syntax.NASM => NasmFormatter
    }
    getFormatted(formatter)
  }

  /** Applies formatter to the instructions and returns serialized x86 program */
  private def getFormatted(f: X86Formatter): String = {
    val s = new StringBuilder()
    s.append(f.fmtPrefix(funLocations))

    //find max string length of instruction (+ 2 default offset)
    val maxLen = instructions.foldRight(0)((i, currMax) => currMax.max(f.fmtInstruction(i).length)) + 2

    //append instructions
    instructions.foreach(instr => {
      val indentedIns = instr match {
        case _: Label => f.fmtInstruction(instr)
        case _ => "  " + f.fmtInstruction(instr)
      }

      s.append(indentedIns)
      instr.comment.map(c => {
        val indent = " " * (1 + maxLen - indentedIns.length)
        s.append(indent)
        s.append("; " + c)
      })
      s.append("\n")
    })

    s.toString()
  }
}
