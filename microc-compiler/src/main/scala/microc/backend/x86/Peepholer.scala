package microc.backend.x86

import microc.backend.x86.helpers._

import scala.annotation.tailrec

/** Performs peephole optimizations on a list of x86 instructions */
object Peepholer {

  /** Performs peephole optimizations on the instructions */
  @tailrec
  def optimize(instructions: List[Instruction]): List[Instruction] = {
    val optimized = run(instructions)
    if (instructions == optimized) {
      instructions
    } else {
      optimize(optimized)
    }
  }

  /** Performs peephole optimizations until fixed-point */
  private def run(instructions: List[Instruction]): List[Instruction] = {
    //peek with 1-instruction window
    val optimized1 = instructions match {
      case first :: rest => peepOne(first) ::: rest
      case _ => instructions
    }

    //peek with 2-instruction window
    val optimized2 = optimized1 match {
      case first :: second :: rest => peepTwo(first, second) ::: rest
      case _ => optimized1
    }

    //move window by one (if rest != Nil)
    optimized2 match {
      case first :: rest if rest.nonEmpty => first :: run(rest)
      case _ => optimized2
    }
  }

  /** Performs 1-instruction window optimizations */
  private def peepOne(first: Instruction): List[Instruction] = first match {
    case AddRegImm(_, Imm(0), _) =>
      List()

    case AddRegImm(r, Imm(1), comment) =>
      List(Inc(r, comment))

    case AddRegImm(r, Imm(-1), comment) =>
      List(Dec(r, comment))

    case AddRegReg(r1, r2, comment) if r1 == r2 =>
      List(ShlRegImm(r1, Imm(1), comment))

    case SubRegImm(_, Imm(0), _) =>
      List()

    case SubRegImm(r, Imm(1), comment) =>
      List(Dec(r, comment))

    case SubRegImm(r, Imm(-1), comment) =>
      List(Inc(r, comment))

    case SubRegReg(r1, r2, comment) if r1 == r2 =>
      List(MovRegImm(r1, Imm(0), comment))

    case MulRegImm(r, Imm(0), comment) =>
      List(MovRegImm(r, Imm(0), comment))

    case MulRegImm(_, Imm(1), _) =>
      List()

    case MulRegImm(r, Imm(n), comment) if isPowerOfTwo(n) =>
      List(ShlRegImm(r, Imm(logBaseTwo(n)), comment))

    case ShlRegImm(_, Imm(0), _) =>
      List()

    case ShrRegImm(_, Imm(0), _) =>
      List()

    case MovRegReg(r1, r2, _) if r1 == r2 =>
      List()

    case MovRegImm(r, Imm(0), comment) =>
      List(XorRegReg(r, r, comment))

    case MovMemReg(MemOffset(r1, 0), r2, comment) =>
      List(MovMemReg(Mem(r1), r2, comment))

    case MovRegMem(r1, MemOffset(r2, 0), comment) =>
      List(MovRegMem(r1, Mem(r2), comment))

    case _ =>
      List(first)
  }

  /** Performs 2-instruction window optimizations */
  private def peepTwo(first: Instruction, second: Instruction): List[Instruction] = (first, second) match {
    case (Jmp(LabelOp(lbl1), _), Label(lbl2, _)) if lbl1 == lbl2 =>
      List(second)

    case (AddRegImm(r1, Imm(n1), c1), AddRegImm(r2, Imm(n2), c2)) if r1 == r2 =>
      List(AddRegImm(r1, Imm(n1 + n2), combine(c1, c2)))

    case (AddRegImm(r1, Imm(n1), c1), SubRegImm(r2, Imm(n2), c2)) if r1 == r2 =>
      List(AddRegImm(r1, Imm(n1 - n2), combine(c1, c2)))

    case (SubRegImm(r1, Imm(n1), c1), SubRegImm(r2, Imm(n2), c2)) if r1 == r2 =>
      List(SubRegImm(r1, Imm(n1 + n2), combine(c1, c2)))

    case (SubRegImm(r1, Imm(n1), c1), AddRegImm(r2, Imm(n2), c2)) if r1 == r2 =>
      List(SubRegImm(r1, Imm(n1 - n2), combine(c1, c2)))

    case (MulRegImm(r1, Imm(n1), c1), MulRegImm(r2, Imm(n2), c2)) if r1 == r2 =>
      List(SubRegImm(r1, Imm(n1 * n2), combine(c1, c2)))

    case (ShlRegImm(r1, Imm(n1), c1), ShlRegImm(r2, Imm(n2), c2)) if r1 == r2 =>
      List(ShlRegImm(r1, Imm(n1 + n2), combine(c1, c2)))

    case (ShlRegImm(r1, Imm(n1), c1), ShrRegImm(r2, Imm(n2), c2)) if r1 == r2 =>
      val diff = n1 - n2
      if (diff >= 0) {
        List(ShlRegImm(r1, Imm(diff), combine(c1, c2)))
      } else {
        List(ShrRegImm(r1, Imm(diff.abs), combine(c1, c2)))
      }

    case (ShrRegImm(r1, Imm(n1), c1), ShrRegImm(r2, Imm(n2), c2)) if r1 == r2 =>
      List(ShrRegImm(r1, Imm(n1 + n2), combine(c1, c2)))

    case (ShrRegImm(r1, Imm(n1), c1), ShlRegImm(r2, Imm(n2), c2)) if r1 == r2 =>
      val diff = n1 - n2
      if (diff >= 0) {
        List(ShrRegImm(r1, Imm(diff), combine(c1, c2)))
      } else {
        List(ShlRegImm(r1, Imm(diff.abs), combine(c1, c2)))
      }

    case (MovRegReg(r1, r2, _), MovRegReg(r3, r4, _)) if r1 == r4 && r2 == r3 =>
      List(first)

    case (MovMemReg(m1, r1, _), MovRegMem(r2, m2, c2)) if m1 == m2 =>
      List(first, MovRegReg(r2, r1, c2))

    case _ =>
      List(first, second)
  }

  /** Returns true if the number is power of 2 */
  private def isPowerOfTwo(n: Int): Boolean = (n & (n - 1)) == 0

  /** Returns logarithm (base 2) of the number */
  private def logBaseTwo(n: Int): Int = n match {
    case 1 => 0
    case _ if n > 1 => 1 + logBaseTwo(n / 2)
    case _ => throw new RuntimeException("Undefined log of base 2")
  }

  /** Combines two instruction comments into one */
  private def combine(c1: Option[String], c2: Option[String]): Option[String] = (c1, c2) match {
    case (Some(l), Some(r)) => Some(l + ", " + r)
    case (Some(_), None) => c1
    case (None, Some(_)) => c2
    case (None, None) => None
  }
}
