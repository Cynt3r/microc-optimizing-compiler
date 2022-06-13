package microc.backend.x86

import microc.backend.x86.helpers._

/** Interface of an x86 assembly formatter */
trait X86Formatter {

  /** Returns x86 assembly prefix containing the data section and extern declarations based on provided map, that maps function names to their memory locations  */
  def fmtPrefix(funLocations: Map[String, String]): String

  /** Returns formatted instruction */
  def fmtInstruction(instr: Instruction): String

  /** Returns formatted operand */
  def fmtOperand(op: Operand): String
}

/**
 * Formatter for the NASM syntax
 * https://www.nasm.us/
 */
object NasmFormatter extends X86Formatter {

  def fmtPrefix(funLocations: Map[String, String]): String = {
    val dataSection =
      """; assemble, link and run with:
        |; nasm -felf64 test.asm && ld --dynamic-linker /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 -lc -o test test.o && ./test
        |section .data
        |printfFmt db "%ld", 10, 0 ; format string for printf()
        |scanfFmt db "%ld", 0 ; format string for scanf()
        |scanfRes times 4 db 0 ; address for scanf() result
        |""".stripMargin
    val functions = funLocations
      .map(loc => loc._2 + ": dd 0,0,0,0")
      .mkString("\n")
    val textSection =
      """
        |
        |section .text
        |global _start
        |extern printf
        |extern scanf
        |extern malloc
        |extern exit
        |
        |""".stripMargin
    dataSection + functions + textSection
  }

  def fmtInstruction(instr: Instruction): String = instr match {
    case i: ZeroOperand => i.name
    case i: OneOperand => s"${i.name} ${fmtOperand(i.op)}"
    case i: TwoOperand => s"${i.name} ${fmtOperand(i.dest)}, ${fmtOperand(i.src)}"
  }

  def fmtOperand(op: Operand): String = op match {
    case Imm(value) => value.toString
    case LabelOp(name) => name
    case reg: Register => reg.name
    case Mem(m) => s"[${fmtOperand(m)}]"
    case MemOffset(m, offset) =>
      val operator = if (offset < 0) "-" else "+"
      s"[${fmtOperand(m)} $operator ${offset.abs}]"
  }
}
