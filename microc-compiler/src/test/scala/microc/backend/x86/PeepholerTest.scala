package microc.backend.x86

import microc.backend.x86.helpers._
import utest._

object PeepholerTest extends TestSuite {
  val tests: Tests = Tests {
    test("Peepholer test 1") {
      val instructions = List(
        AddRegImm(RDI(X86RegMode.M64), Imm(1)),
        Jmp(LabelOp("bb1")),
        AddRegImm(RAX(X86RegMode.M64), Imm(5)),
        SubRegImm(RAX(X86RegMode.M64), Imm(8)),
        AddRegImm(RAX(X86RegMode.M64), Imm(3)),
        MovRegReg(RBX(X86RegMode.M64), RBX(X86RegMode.M64)),
        Label("bb1"),
      )
      Peepholer.optimize(instructions) ==> List(
        Inc(RDI(X86RegMode.M64)),
        Label("bb1"),
      )
    }

    test("Peepholer test 2") {
      val instructions = List(
        MovRegReg(RAX(X86RegMode.M64), RBX(X86RegMode.M64)),
        MovRegReg(RBX(X86RegMode.M64), RAX(X86RegMode.M64)),
        MovRegImm(RDI(X86RegMode.M64), Imm(0)),
        MulRegImm(RBX(X86RegMode.M64), Imm(2)),
        MulRegImm(RBX(X86RegMode.M64), Imm(4)),
      )
      Peepholer.optimize(instructions) ==> List(
        MovRegReg(RAX(X86RegMode.M64), RBX(X86RegMode.M64)),
        XorRegReg(RDI(X86RegMode.M64), RDI(X86RegMode.M64)),
        ShlRegImm(RBX(X86RegMode.M64), Imm(3)),
      )
    }

    test("Peepholer test 3") {
      val instructions = List(
        ShlRegImm(RAX(X86RegMode.M64), Imm(1)),
        MulRegImm(RBX(X86RegMode.M64), Imm(1)),
        SubRegImm(RDI(X86RegMode.M64), Imm(0)),
        ShrRegImm(RAX(X86RegMode.M64), Imm(4)),
      )
      Peepholer.optimize(instructions) ==> List(
        ShrRegImm(RAX(X86RegMode.M64), Imm(3))
      )
    }

    test("Peepholer comment combining") {
      val instructions = List(
        SubRegImm(RAX(X86RegMode.M64), Imm(2), Some("minus 2")),
        AddRegImm(RAX(X86RegMode.M64), Imm(4), Some("plus 4")),
        SubRegImm(RAX(X86RegMode.M64), Imm(8)),
      )
      Peepholer.optimize(instructions) ==> List(SubRegImm(RAX(X86RegMode.M64), Imm(6), Some("minus 2, plus 4")))
    }
  }
}
