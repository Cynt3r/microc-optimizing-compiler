object PeepholerTest extends TestSuite {
val tests: Tests = Tests {
    test("Peepholer test 1") {
        //input
        val instructions = List(
            AddRegImm(RDI(X86RegMode.M64), Imm(1)),
            Jmp(LabelOp("bb1")),
            AddRegImm(RAX(X86RegMode.M64), Imm(5)),
            SubRegImm(RAX(X86RegMode.M64), Imm(8)),
            AddRegImm(RAX(X86RegMode.M64), Imm(3)),
            MovRegReg(RBX(X86RegMode.M64), RBX(X86RegMode.M64)),
            Label("bb1"),
        )
        
        //assert
        Peepholer.optimize(instructions) ==> List(
            Inc(RDI(X86RegMode.M64)),
            Label("bb1"),
        )
    }
}
}