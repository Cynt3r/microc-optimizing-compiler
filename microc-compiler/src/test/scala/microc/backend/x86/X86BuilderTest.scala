package microc.backend.x86

import microc.X86Syntax
import microc.backend.x86.helpers._
import utest._

object X86BuilderTest extends TestSuite {
  val tests: Tests = Tests {
    test("Basic NASMx86 builder test") {
      val b = new X86Builder()
      b.registerFun("main", "__main")
      b.add(Label("bb0"))
      b.add(MovRegReg(RAX(X86RegMode.M64), RBX(X86RegMode.M64)))
      b.add(MovMemReg(MemOffset(RAX(X86RegMode.M64), -5), RAX(X86RegMode.M64)))
      b.add(Jmp(LabelOp("bb1")))
      b.add(Label("bb1"))
      b.add(AddRegImm(R8(X86RegMode.M64), Imm(10)))
      b.add(Ret())
      b.patch(5, AddRegImm(R8(X86RegMode.M64), Imm(15)))

      val actual = b.build(X86Syntax.NASM, optimize = false)
      b.funMemLocation("main") ==> "__main"

      val expected =
        """; assemble, link and run with:
          |; nasm -felf64 test.asm && ld --dynamic-linker /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 -lc -o test test.o && ./test
          |section .data
          |printfFmt db "%ld", 10, 0 ; format string for printf()
          |scanfFmt db "%ld", 0 ; format string for scanf()
          |scanfRes times 4 db 0 ; address for scanf() result
          |__main: dd 0,0,0,0
          |
          |section .text
          |global _start
          |extern printf
          |extern scanf
          |extern malloc
          |extern exit
          |
          |bb0:
          |  mov rax, rbx
          |  mov [rax - 5], rax
          |  jmp bb1
          |bb1:
          |  add r8, 15
          |  ret
          |""".stripMargin

      actual ==> expected
    }
  }
}
