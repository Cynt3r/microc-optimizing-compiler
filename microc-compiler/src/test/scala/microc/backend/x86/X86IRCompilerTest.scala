package microc.backend.x86

import microc.backend.BackendHelper
import microc.backend.x86.helpers._
import utest._

object X86IRCompilerTest extends TestSuite with BackendHelper {
  val tests: Tests = Tests {
    /*
    test("Basic x86IR to X86 compilation") {
      val instructions = codeToX86(basicProgram).toArray

      assertMatch(instructions(0)){case Label("_start", _) =>}
      assertMatch(instructions(1)){case MovRegLabel(RAX(X86RegMode.M64), LabelOp("main"), _) =>}
      assertMatch(instructions(2)){case MovMemReg(Mem(LabelOp("__main")), RAX(X86RegMode.M64), _) =>}
      assertMatch(instructions(3)){case CallLabel(LabelOp("main"), _) =>}
      assertMatch(instructions(4)){case CallLabel(LabelOp("exit"), _) =>}
      assertMatch(instructions(5)){case Label("main", _) =>}
      assertMatch(instructions(6)){case PushReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(7)){case MovRegReg(RBP(X86RegMode.M64), RSP(X86RegMode.M64), _) =>}
      assertMatch(instructions(8)){case SubRegImm(RSP(X86RegMode.M64), Imm(32), _) =>}
      assertMatch(instructions(9)){case Label(_, _) =>}
      //load 3
      assertMatch(instructions(10)){case MovRegImm(_, Imm(3), _) =>}
      val imm3 = instructions(10).asInstanceOf[MovRegImm].dest
      //x <- 3
      assertMatch(instructions(11)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), r, _) if r == imm3 =>}
      //load x
      assertMatch(instructions(12)){case MovRegMem(_, MemOffset(RBP(X86RegMode.M64), -8), _) =>}
      val loadX = instructions(12).asInstanceOf[MovRegMem].dest
      //load 2
      assertMatch(instructions(13)){case MovRegImm(_, Imm(2), _) =>}
      val imm2 = instructions(13).asInstanceOf[MovRegImm].dest
      //load 2 + x
      assertMatch(instructions(14)){case MovRegReg(_, r, _) if r == imm2 =>}
      val addInner = instructions(14).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(15)){case AddRegReg(r1, r2, _) if r1 == addInner && r2 == loadX =>}
      //load 1
      assertMatch(instructions(16)){case MovRegImm(_, Imm(1), _) =>}
      val imm1 = instructions(16).asInstanceOf[MovRegImm].dest
      //load 1 + 2 + x
      assertMatch(instructions(17)){case MovRegReg(_, r, _) if r == imm1 =>}
      val addOuter = instructions(17).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(18)){case AddRegReg(r1, r2, _) if r1 == addOuter && r2 == addInner =>}
      //y <- 1 + 2 + x
      assertMatch(instructions(19)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -16), r, _) if r == addOuter =>}
      //load 8
      assertMatch(instructions(20)){case MovRegImm(_, Imm(8), _) =>}
      val imm8 = instructions(20).asInstanceOf[MovRegImm].dest
      //load y
      assertMatch(instructions(21)){case MovRegMem(_, MemOffset(RBP(X86RegMode.M64), -16), _) =>}
      val loadY = instructions(21).asInstanceOf[MovRegMem].dest
      //load y * 8
      assertMatch(instructions(22)){case MovRegReg(_, r, _) if r == loadY =>}
      val mul = instructions(22).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(23)){case MulRegReg(r1, r2, _) if r1 == mul && r2 == imm8 =>}
      //z <- y * 8
      assertMatch(instructions(24)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -24), r, _) if r == mul =>}
      //load z
      assertMatch(instructions(25)){case MovRegMem(_, MemOffset(RBP(X86RegMode.M64), -24), _) =>}
      val loadZ = instructions(25).asInstanceOf[MovRegMem].dest
      //return z
      assertMatch(instructions(26)){case MovRegReg(RAX(X86RegMode.M32), r, _) if r == loadZ =>}
      assertMatch(instructions(27)){case MovRegReg(RSP(X86RegMode.M64), RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(28)){case PopReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(29)){case Ret(_) =>}
    }

    test("x86IR with if statement to x86 compilation") {
      val instructions = codeToX86(ifProgram).toArray

      assertMatch(instructions(5)){case Label("main", _) =>}
      assertMatch(instructions(6)){case PushReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(7)){case MovRegReg(RBP(X86RegMode.M64), RSP(X86RegMode.M64), _) =>}
      assertMatch(instructions(8)){case SubRegImm(RSP(X86RegMode.M64), Imm(16), _) =>}
      assertMatch(instructions(9)){case Label(_, _) =>}
      //load 1
      assertMatch(instructions(10)){case MovRegImm(_, Imm(1), _) =>}
      val imm1 = instructions(10).asInstanceOf[MovRegImm].dest
      assertMatch(instructions(11)){case CmpRegImm(r, Imm(0), _) if r == imm1 =>}
      assertMatch(instructions(12)){case Je(LabelOp(_), _) =>}
      val elseLbl = instructions(12).asInstanceOf[Je].op.name
      assertMatch(instructions(13)){case Jmp(LabelOp(_), _) =>}
      val thenLbl = instructions(13).asInstanceOf[Jmp].op.name
      //then
      assertMatch(instructions(14)){case Label(lbl, _) if lbl == thenLbl =>}
      //load 42
      assertMatch(instructions(15)){case MovRegImm(_, Imm(42), _) =>}
      val imm42 = instructions(15).asInstanceOf[MovRegImm].dest
      //x <- 42
      assertMatch(instructions(16)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), r, _) if r == imm42 =>}
      //jump -> finally
      assertMatch(instructions(17)){case Jmp(_, _) =>}
      val finallyLbl = instructions(17).asInstanceOf[Jmp].op.name
      //else
      assertMatch(instructions(18)){case Label(lbl, _) if lbl == elseLbl =>}
      //load -42
      assertMatch(instructions(19)){case MovRegImm(_, Imm(-42), _) =>}
      val immNeg42 = instructions(19).asInstanceOf[MovRegImm].dest
      //x <- -42
      assertMatch(instructions(20)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), r, _) if r == immNeg42 =>}
      //jump -> finally
      assertMatch(instructions(21)){case Jmp(LabelOp(lbl), _) if lbl == finallyLbl =>}
      //finally
      assertMatch(instructions(22)){case Label(lbl, _) if lbl == finallyLbl =>}
      //load x
      assertMatch(instructions(23)){case MovRegMem(_, MemOffset(RBP(X86RegMode.M64), -8), _) =>}
      val loadX = instructions(23).asInstanceOf[MovRegMem].dest
      //return x
      assertMatch(instructions(24)){case MovRegReg(RAX(X86RegMode.M32), r, _) if r == loadX =>}
      assertMatch(instructions(25)){case MovRegReg(RSP(X86RegMode.M64), RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(26)){case PopReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(27)){case Ret(_) =>}
    }

    test("x86IR with while statement to x86 compilation") {
      val instructions = codeToX86(whileProgram).toArray

      assertMatch(instructions(5)){case Label("main", _) =>}
      assertMatch(instructions(6)){case PushReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(7)){case MovRegReg(RBP(X86RegMode.M64), RSP(X86RegMode.M64), _) =>}
      assertMatch(instructions(8)){case SubRegImm(RSP(X86RegMode.M64), Imm(16), _) =>}
      assertMatch(instructions(9)){case Label(_, _) =>}
      assertMatch(instructions(10)){case Jmp(_, _) =>}
      val guardLbl = instructions(10).asInstanceOf[Jmp].op.name
      //guard
      assertMatch(instructions(11)){case Label(lbl, _) if lbl == guardLbl =>}
      //load 1
      assertMatch(instructions(12)){case MovRegImm(_, Imm(1), _) =>}
      val imm1 = instructions(12).asInstanceOf[MovRegImm].dest
      assertMatch(instructions(13)){case CmpRegImm(r, Imm(0), _) if r == imm1 =>}
      assertMatch(instructions(14)){case Je(LabelOp(_), _) =>}
      val afterLbl = instructions(14).asInstanceOf[Je].op.name
      assertMatch(instructions(15)){case Jmp(LabelOp(_), _) =>}
      val bodyLbl = instructions(15).asInstanceOf[Jmp].op.name
      //body
      assertMatch(instructions(16)){case Label(lbl, _) if lbl == bodyLbl =>}
      //load 42
      assertMatch(instructions(17)){case MovRegImm(_, Imm(42), _) =>}
      val imm42 = instructions(17).asInstanceOf[MovRegImm].dest
      //x <- 42
      assertMatch(instructions(18)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), r, _) if r == imm42 =>}
      //jump -> finally
      assertMatch(instructions(19)){case Jmp(LabelOp(lbl), _) if lbl == guardLbl =>}
      //after
      assertMatch(instructions(20)){case Label(lbl, _) if lbl == afterLbl =>}
      //load x
      assertMatch(instructions(21)){case MovRegMem(_, MemOffset(RBP(X86RegMode.M64), -8), _) =>}
      val loadX = instructions(21).asInstanceOf[MovRegMem].dest
      //return x
      assertMatch(instructions(22)){case MovRegReg(RAX(X86RegMode.M32), r, _) if r == loadX =>}
      assertMatch(instructions(23)){case MovRegReg(RSP(X86RegMode.M64), RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(24)){case PopReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(25)){case Ret(_) =>}
    }

    test("x86IR with function call to x86 compilation") {
      val instructions = codeToX86(funProgram).toArray

      assertMatch(instructions(0)){case Label("_start", _) =>}
      assertMatch(instructions(1)){case MovRegLabel(RAX(X86RegMode.M64), LabelOp("foo"), _) =>}
      assertMatch(instructions(2)){case MovMemReg(Mem(LabelOp("__foo")), RAX(X86RegMode.M64), _) =>}
      assertMatch(instructions(3)){case MovRegLabel(RAX(X86RegMode.M64), LabelOp("main"), _) =>}
      assertMatch(instructions(4)){case MovMemReg(Mem(LabelOp("__main")), RAX(X86RegMode.M64), _) =>}
      assertMatch(instructions(5)){case CallLabel(LabelOp("main"), _) =>}
      assertMatch(instructions(6)){case CallLabel(LabelOp("exit"), _) =>}

      //foo
      assertMatch(instructions(7)){case Label("foo", _) =>}
      assertMatch(instructions(8)){case PushReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(9)){case MovRegReg(RBP(X86RegMode.M64), RSP(X86RegMode.M64), _) =>}
      assertMatch(instructions(10)){case SubRegImm(RSP(X86RegMode.M64), Imm(16), _) =>}
      assertMatch(instructions(11)){case Label(_, _) =>}
      val bbName = instructions(11).asInstanceOf[Label].name
      //load b
      assertMatch(instructions(12)){case MovRegMem(_, MemOffset(RBP(X86RegMode.M64), 16), _) =>}
      val loadB = instructions(12).asInstanceOf[MovRegMem].dest
      //load a
      assertMatch(instructions(13)){case MovRegMem(_, MemOffset(RBP(X86RegMode.M64), 24), _) =>}
      val loadA = instructions(13).asInstanceOf[MovRegMem].dest
      //load a + b
      assertMatch(instructions(14)){case MovRegReg(_, r, _) if r == loadA =>}
      val plus = instructions(14).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(15)){case AddRegReg(r1, r2, _) if r1 == plus && r2 == loadB =>}
      //c <- a + b
      assertMatch(instructions(16)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), r, _) if r == plus =>}
      //load c
      assertMatch(instructions(17)){case MovRegMem(_, MemOffset(RBP(X86RegMode.M64), -8), _) =>}
      val loadC = instructions(17).asInstanceOf[MovRegMem].dest
      //return
      assertMatch(instructions(18)){case MovRegReg(RAX(X86RegMode.M32), r, _) if r == loadC =>}
      assertMatch(instructions(19)){case MovRegReg(RSP(X86RegMode.M64), RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(20)){case PopReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(21)){case Ret(_) =>}

      //main
      assertMatch(instructions(22)){case Label("main", _) =>}
      assertMatch(instructions(23)){case PushReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(24)){case MovRegReg(RBP(X86RegMode.M64), RSP(X86RegMode.M64), _) =>}
      assertMatch(instructions(25)){case SubRegImm(RSP(X86RegMode.M64), Imm(16), _) =>}
      assertMatch(instructions(26)){case Label(name, _) if name != bbName =>}
      //load function foo
      assertMatch(instructions(27)){case MovRegMem(_, Mem(LabelOp("__foo")), _) =>}
      val foo = instructions(27).asInstanceOf[MovRegMem].dest
      //load 19
      assertMatch(instructions(28)){case MovRegImm(_, Imm(19), _) =>}
      val imm19 = instructions(28).asInstanceOf[MovRegImm].dest
      //load 23
      assertMatch(instructions(29)){case MovRegImm(_, Imm(23), _) =>}
      val imm23 = instructions(29).asInstanceOf[MovRegImm].dest
      //move load foo that got loaded into RAX
      assertMatch(instructions(30)){case MovRegReg(_, r, _) if r == foo =>}
      val newFoo = instructions(30).asInstanceOf[MovRegReg].dest
      //persist registers
      assertMatch(instructions(31)){case PushReg(r, _) if r.id == imm19.id && r.mode == X86RegMode.M64 =>}
      assertMatch(instructions(32)){case PushReg(r, _) if r.id == imm23.id && r.mode == X86RegMode.M64 =>}
      assertMatch(instructions(33)){case PushReg(r, _) if r.id == newFoo.id && r.mode == X86RegMode.M64 =>}
      //stack align
      assertMatch(instructions(34)){case SubRegImm(RSP(X86RegMode.M64), Imm(8), _) =>}
      //load arguments
      assertMatch(instructions(35)){case PushReg(r, _) if r.id == imm19.id && r.mode == X86RegMode.M64 =>}
      assertMatch(instructions(36)){case PushReg(r, _) if r.id == imm23.id && r.mode == X86RegMode.M64 =>}
      //call
      assertMatch(instructions(37)){case CallReg(r, _) if r == newFoo =>}
      //delete arguments
      assertMatch(instructions(38)){case AddRegImm(RSP(X86RegMode.M64), Imm(16), _) =>}
      //dealign stack
      assertMatch(instructions(39)){case AddRegImm(RSP(X86RegMode.M64), Imm(8), _) =>}
      //reload registers
      assertMatch(instructions(40)){case PopReg(r, _) if r.id == newFoo.id && r.mode == X86RegMode.M64 =>}
      assertMatch(instructions(41)){case PopReg(r, _) if r.id == imm23.id && r.mode == X86RegMode.M64 =>}
      assertMatch(instructions(42)){case PopReg(r, _) if r.id == imm19.id && r.mode == X86RegMode.M64 =>}
      //x <- call
      assertMatch(instructions(43)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), RAX(X86RegMode.M32), _) =>}
      //load x
      assertMatch(instructions(44)){case MovRegMem(_, MemOffset(RBP(X86RegMode.M64), -8), _) =>}
      val loadX = instructions(44).asInstanceOf[MovRegMem].dest
      //return
      assertMatch(instructions(45)){case MovRegReg(RAX(X86RegMode.M32), r, _) if r == loadX =>}
      assertMatch(instructions(46)){case MovRegReg(RSP(X86RegMode.M64), RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(47)){case PopReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(48)){case Ret(_) =>}
    }

    test("x86IR with composed values to x86 compilation") {
      val instructions = codeToX86(recProgram).toArray

      assertMatch(instructions(5)){case Label("main", _) =>}
      assertMatch(instructions(6)){case PushReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(7)){case MovRegReg(RBP(X86RegMode.M64), RSP(X86RegMode.M64), _) =>}
      assertMatch(instructions(8)){case SubRegImm(RSP(X86RegMode.M64), Imm(80), _) =>}
      assertMatch(instructions(9)){case Label(_, _) =>}
      //load 8
      assertMatch(instructions(10)){case MovRegImm(_, Imm(8), _) =>}
      val imm8 = instructions(10).asInstanceOf[MovRegImm].dest
      //load 9
      assertMatch(instructions(11)){case MovRegImm(_, Imm(9), _) =>}
      val imm9 = instructions(11).asInstanceOf[MovRegImm].dest
      //load {c: 8, d: 9}
      assertMatch(instructions(12)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -48), r, _) if r == imm8=>}
      assertMatch(instructions(13)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -56), r, _) if r == imm9=>}
      //save address of {c: 8, d: 9}
      assertMatch(instructions(14)){case MovRegReg(_, RBP(X86RegMode.M64), _) =>}
      val rec1 = instructions(14).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(15)){case AddRegImm(r, Imm(-48), _) if r == rec1 =>}
      //load 5
      assertMatch(instructions(16)){case MovRegImm(_, Imm(5), _) =>}
      val imm5 = instructions(16).asInstanceOf[MovRegImm].dest
      //load {a: {c: 8, d: 9}, b: 5}
      assertMatch(instructions(17)){case MovRegMem(_, MemOffset(r, 0), _) if r == rec1 =>}
      val workReg1 = instructions(17).asInstanceOf[MovRegMem].dest
      assertMatch(instructions(18)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -64), r, _) if r == workReg1 =>}
      assertMatch(instructions(19)){case MovRegMem(_, MemOffset(r, -8), _) if r == rec1 =>}
      assertMatch(instructions(20)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -72), r, _) if r == workReg1 =>}
      assertMatch(instructions(21)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -80), r, _) if r == imm5 =>}
      //save address of {a: {c: 8, d: 9}, b: 5}
      assertMatch(instructions(22)){case MovRegReg(_, RBP(X86RegMode.M64), _) =>}
      val rec2 = instructions(22).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(23)){case AddRegImm(r, Imm(-64), _) if r == rec2 =>}
      //x <- {a: {c: 8, d: 9}, b: 5}
      assertMatch(instructions(24)){case MovRegMem(_, MemOffset(r, 0), _) if r == rec2 =>}
      val workReg2 = instructions(24).asInstanceOf[MovRegMem].dest
      assertMatch(instructions(25)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), r, _) if r == workReg2 =>}
      assertMatch(instructions(26)){case MovRegMem(_, MemOffset(r, -8), _) if r == rec2 =>}
      assertMatch(instructions(27)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -16), r, _) if r == workReg2 =>}
      assertMatch(instructions(28)){case MovRegMem(_, MemOffset(r, -16), _) if r == rec2 =>}
      assertMatch(instructions(29)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -24), r, _) if r == workReg2 =>}
      //load offset of "b"
      assertMatch(instructions(30)){case MovRegImm(_, Imm(-16), _) =>}
      val offsetB = instructions(30).asInstanceOf[MovRegImm].dest
      //load address of x
      assertMatch(instructions(31)){case MovRegReg(_, RBP(X86RegMode.M64), _) =>}
      val loadXb = instructions(31).asInstanceOf[MovRegReg].dest
      //add offset of "b" to x
      assertMatch(instructions(32)){case AddRegImm(r, Imm(-8), _) if r == loadXb =>}
      assertMatch(instructions(33)){case AddRegReg(r1, r2, _) if r1 == loadXb && r2 == offsetB =>}
      //load 42
      assertMatch(instructions(34)){case MovRegImm(_, Imm(42), _) =>}
      val imm42 = instructions(34).asInstanceOf[MovRegImm].dest
      //x.b <- 42
      assertMatch(instructions(35)){case MovMemReg(Mem(r1), r2, _) if r1 == loadXb && r2 == imm42 =>}
      //load x address
      assertMatch(instructions(36)){case MovRegReg(_, RBP(X86RegMode.M64), _) =>}
      val loadX = instructions(36).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(37)){case AddRegImm(r, Imm(-8), _) if r == loadX =>}
      //load offset of "a"
      assertMatch(instructions(38)){case MovRegImm(_, Imm(0), _) =>}
      val offsetA = instructions(38).asInstanceOf[MovRegImm].dest
      //load x address
      assertMatch(instructions(39)){case MovRegReg(_, r, _) if r == loadX =>}
      val addrXa = instructions(39).asInstanceOf[MovRegReg].dest
      //add offset of "b" to x
      assertMatch(instructions(40)){case AddRegReg(r1, r2, _) if r1 == addrXa && r2 == offsetA =>}
      assertMatch(instructions(41)){case MovRegReg(_, r, _) if r == addrXa =>}
      val loadXa = instructions(41).asInstanceOf[MovRegReg].dest
      //y <- x.a
      assertMatch(instructions(42)){case MovRegMem(_, MemOffset(r, 0), _) if r == loadXa =>}
      val workReg3 = instructions(42).asInstanceOf[MovRegMem].dest
      assertMatch(instructions(43)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -32), r, _) if r == workReg3 =>}
      assertMatch(instructions(44)){case MovRegMem(r1, MemOffset(r2, -8), _) if r1 == workReg3 && r2 == loadXa =>}
      assertMatch(instructions(45)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -40), r, _) if r == workReg3 =>}
      //load 0
      assertMatch(instructions(46)){case MovRegImm(_, Imm(0), _) =>}
      val imm0 = instructions(46).asInstanceOf[MovRegImm].dest
      //return
      assertMatch(instructions(47)){case MovRegReg(RAX(X86RegMode.M32), r, _) if r == imm0 =>}
      assertMatch(instructions(48)){case MovRegReg(RSP(X86RegMode.M64), RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(49)){case PopReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(50)){case Ret(_) =>}
    }

    test("x86IR with function calls using composed values to x86 compilation") {
      val instructions = codeToX86(funRecProgram).toArray

      //foo
      assertMatch(instructions(7)){case Label("foo", _) =>}
      assertMatch(instructions(8)){case PushReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(9)){case MovRegReg(RBP(X86RegMode.M64), RSP(X86RegMode.M64), _) =>}
      assertMatch(instructions(10)){case SubRegImm(RSP(X86RegMode.M64), Imm(16), _) =>}
      assertMatch(instructions(11)){case Label(_, _) =>}
      //load rec address
      assertMatch(instructions(12)){case MovRegReg(_, RBP(X86RegMode.M64), _) =>}
      var loadRec = instructions(12).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(13)){case AddRegImm(r, Imm(24), _) if r == loadRec =>}
      //load offset of "b"
      assertMatch(instructions(14)){case MovRegImm(_, Imm(-8), _) =>}
      val offsetB = instructions(14).asInstanceOf[MovRegImm].dest
      //load rec address
      assertMatch(instructions(15)){case MovRegReg(_, r, _) if r == loadRec =>}
      val addrRecB = instructions(15).asInstanceOf[MovRegReg].dest
      //add offset of "b" to rec
      assertMatch(instructions(16)){case AddRegReg(r1, r2, _) if r1 == addrRecB && r2 == offsetB =>}
      //load rec.b
      assertMatch(instructions(17)){case MovRegMem(_, Mem(r), _) if r == addrRecB =>}
      val loadRecB = instructions(17).asInstanceOf[MovRegMem].dest
      //load rec address
      assertMatch(instructions(18)){case MovRegReg(_, RBP(X86RegMode.M64), _) =>}
      loadRec = instructions(18).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(19)){case AddRegImm(r, Imm(24), _) if r == loadRec =>}
      //load offset of "a"
      assertMatch(instructions(20)){case MovRegImm(_, Imm(0), _) =>}
      val offsetA = instructions(20).asInstanceOf[MovRegImm].dest
      //load rec address
      assertMatch(instructions(21)){case MovRegReg(_, r, _) if r == loadRec =>}
      val addrRecA = instructions(21).asInstanceOf[MovRegReg].dest
      //add offset of "b" to rec
      assertMatch(instructions(22)){case AddRegReg(r1, r2, _) if r1 == addrRecA && r2 == offsetA =>}
      //load rec.a
      assertMatch(instructions(23)){case MovRegMem(_, Mem(r), _) if r == addrRecA =>}
      val loadRecA = instructions(23).asInstanceOf[MovRegMem].dest
      //load {a: rec.b, b: rec.a}
      assertMatch(instructions(24)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), r, _) if r == loadRecB =>}
      assertMatch(instructions(25)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -16), r, _) if r == loadRecA =>}
      //save address of {a: rec.b, b: rec.a}
      assertMatch(instructions(26)){case MovRegReg(_, RBP(X86RegMode.M64), _) =>}
      val loadRec1 = instructions(26).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(27)){case AddRegImm(r, Imm(-8), _) if r == loadRec1 =>}
      //return
      assertMatch(instructions(28)){case MovRegReg(RAX(X86RegMode.M64), r, _) if r == loadRec1 =>}
      assertMatch(instructions(29)){case MovRegReg(RSP(X86RegMode.M64), RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(30)){case PopReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(31)){case Ret(_) =>}

      //main
      assertMatch(instructions(32)){case Label("main", _) =>}
      assertMatch(instructions(33)){case PushReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(34)){case MovRegReg(RBP(X86RegMode.M64), RSP(X86RegMode.M64), _) =>}
      assertMatch(instructions(35)){case SubRegImm(RSP(X86RegMode.M64), Imm(32), _) =>}
      assertMatch(instructions(36)){case Label(_, _) =>}
      //load 1
      assertMatch(instructions(37)){case MovRegImm(_, Imm(1), _) =>}
      val imm1 = instructions(37).asInstanceOf[MovRegImm].dest
      //load 2
      assertMatch(instructions(38)){case MovRegImm(_, Imm(2), _) =>}
      val imm2 = instructions(38).asInstanceOf[MovRegImm].dest
      //load {a: 1, b: 2}
      assertMatch(instructions(39)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -24), r, _) if r == imm1 =>}
      assertMatch(instructions(40)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -32), r, _) if r == imm2 =>}
      //save address of {a: 1, b: 2}
      assertMatch(instructions(41)){case MovRegReg(_, RBP(X86RegMode.M64), _) =>}
      val loadRec2 = instructions(41).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(42)){case AddRegImm(r, Imm(-24), _) if r == loadRec2 =>}
      //x <- {a: 1, b: 2}
      assertMatch(instructions(43)){case MovRegMem(_, MemOffset(r, 0), _) if r == loadRec2 =>}
      val workReg1 = instructions(43).asInstanceOf[MovRegMem].dest
      assertMatch(instructions(44)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), r, _) if r == workReg1 =>}
      assertMatch(instructions(45)){case MovRegMem(_, MemOffset(r, -8), _) if r == loadRec2 =>}
      assertMatch(instructions(46)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -16), r, _) if r == workReg1 =>}
      //load foo
      assertMatch(instructions(47)){case MovRegMem(_, Mem(LabelOp("__foo")), _) =>}
      val foo = instructions(47).asInstanceOf[MovRegMem].dest
      //load x
      assertMatch(instructions(48)){case MovRegReg(_, RBP(X86RegMode.M64), _) =>}
      val loadX = instructions(48).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(49)){case AddRegImm(r, Imm(-8), _) if r == loadX =>}
      //move foo that got loaded into RAX
      assertMatch(instructions(50)){case MovRegReg(_, r, _) if r == foo =>}
      val fooNew = instructions(50).asInstanceOf[MovRegReg].dest
      //persist registers
      assertMatch(instructions(51)){case PushReg(r, _) if r == loadX =>}
      assertMatch(instructions(52)){case PushReg(r, _) if r == fooNew =>}
      //load arguments
      assertMatch(instructions(53)){case MovRegMem(_, MemOffset(r, 0), _) if r == loadX =>}
      val workReg2 = instructions(53).asInstanceOf[MovRegMem].dest
      assertMatch(instructions(54)){case PushReg(r, _) if r == workReg2 =>}
      assertMatch(instructions(55)){case MovRegMem(_, MemOffset(r, -8), _) if r == loadX =>}
      assertMatch(instructions(56)){case PushReg(r, _) if r == workReg2 =>}
      //call
      assertMatch(instructions(57)){case CallReg(r, _) if r == fooNew =>}
      //delete arguments
      assertMatch(instructions(58)){case AddRegImm(RSP(X86RegMode.M64), Imm(16), _) =>}
      //reload registers
      assertMatch(instructions(59)){case PopReg(r, _) if r == fooNew =>}
      assertMatch(instructions(60)){case PopReg(r, _) if r == loadX =>}
      //copy record to current stack frame
      assertMatch(instructions(61)){case MovRegMem(_, MemOffset(RAX(X86RegMode.M64), 0), _) =>}
      val workReg3 = instructions(61).asInstanceOf[MovRegMem].dest
      assertMatch(instructions(62)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -24), r, _) if r == workReg3 =>}
      assertMatch(instructions(63)){case MovRegMem(r, MemOffset(RAX(X86RegMode.M64), -8), _) if r == workReg3 =>}
      assertMatch(instructions(64)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -32), r, _) if r == workReg3 =>}
      assertMatch(instructions(65)){case MovRegReg(RAX(X86RegMode.M64), RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(66)){case AddRegImm(RAX(X86RegMode.M64), Imm(-24), _) =>}
      //x <- call
      assertMatch(instructions(67)){case MovRegMem(_, MemOffset(RAX(X86RegMode.M64), 0), _) =>}
      val workReg4 = instructions(67).asInstanceOf[MovRegMem].dest
      assertMatch(instructions(68)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), r, _) if r == workReg4 =>}
      assertMatch(instructions(69)){case MovRegMem(r, MemOffset(RAX(X86RegMode.M64), -8), _) if r == workReg4 =>}
      assertMatch(instructions(70)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -16), r, _) if r == workReg4 =>}
      //load 0
      assertMatch(instructions(71)){case MovRegImm(_, Imm(0), _) =>}
      val imm0 = instructions(71).asInstanceOf[MovRegImm].dest
      //return
      assertMatch(instructions(72)){case MovRegReg(RAX(X86RegMode.M32), r, _) if r == imm0 =>}
      assertMatch(instructions(73)){case MovRegReg(RSP(X86RegMode.M64), RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(74)){case PopReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(75)){case Ret(_) =>}
    }

    test("x86IR with pointers to x86 compilation") {
      val instructions = codeToX86(pointProgram).toArray

      assertMatch(instructions(5)){case Label("main", _) =>}
      assertMatch(instructions(6)){case PushReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(7)){case MovRegReg(RBP(X86RegMode.M64), RSP(X86RegMode.M64), _) =>}
      assertMatch(instructions(8)){case SubRegImm(RSP(X86RegMode.M64), Imm(16), _) =>}
      assertMatch(instructions(9)){case Label(_, _) =>}
      //load 5
      assertMatch(instructions(10)){case MovRegImm(_, Imm(5), _) =>}
      val imm5 = instructions(10).asInstanceOf[MovRegImm].dest
      assertMatch(instructions(11)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), r, _) if r == imm5 =>}
      //load x address
      assertMatch(instructions(12)){case MovRegReg(_, RBP(X86RegMode.M64), _) =>}
      val addrX = instructions(12).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(13)){case AddRegImm(r, Imm(-8), _) if r == addrX =>}
      //y <- &x
      assertMatch(instructions(14)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -16), r, _) if r == addrX =>}
      //load y containing an address of x
      assertMatch(instructions(15)){case MovRegMem(_, MemOffset(RBP(X86RegMode.M64), -16), _) =>}
      val loadY1 = instructions(15).asInstanceOf[MovRegMem].dest
      //load 8
      assertMatch(instructions(16)){case MovRegImm(_, Imm(8), _) =>}
      val imm8 = instructions(16).asInstanceOf[MovRegImm].dest
      // *y <- 8
      assertMatch(instructions(17)){case MovMemReg(Mem(r1), r2, _) if r1 == loadY1 && r2 == imm8 =>}
      //load y
      assertMatch(instructions(18)){case MovRegMem(_, MemOffset(RBP(X86RegMode.M64), -16), _) =>}
      val loadY2 = instructions(18).asInstanceOf[MovRegMem].dest
      //load *y
      assertMatch(instructions(19)){case MovRegMem(_, Mem(r), _) if r == loadY2 =>}
      val loadPY = instructions(19).asInstanceOf[MovRegMem].dest
      //x <- *y
      assertMatch(instructions(20)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), r, _) if r == loadPY =>}
      //load null
      assertMatch(instructions(21)){case MovRegImm(_, Imm(0), _) =>}
      val nul = instructions(21).asInstanceOf[MovRegImm].dest
      //y <- null
      assertMatch(instructions(22)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -16), r, _) if r == nul =>}
      //load 0
      assertMatch(instructions(23)){case MovRegImm(_, Imm(0), _) =>}
      val imm0 = instructions(23).asInstanceOf[MovRegImm].dest
      //return x
      assertMatch(instructions(24)){case MovRegReg(RAX(X86RegMode.M32), r, _) if r == imm0 =>}
      assertMatch(instructions(25)){case MovRegReg(RSP(X86RegMode.M64), RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(26)){case PopReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(27)){case Ret(_) =>}
    }

    test("x86IR with pointers to composed values to x86 compilation") {
      val instructions = codeToX86(pointRecProgram).toArray

      assertMatch(instructions(5)){case Label("main", _) =>}
      assertMatch(instructions(6)){case PushReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(7)){case MovRegReg(RBP(X86RegMode.M64), RSP(X86RegMode.M64), _) =>}
      assertMatch(instructions(8)){case SubRegImm(RSP(X86RegMode.M64), Imm(32), _) =>}
      assertMatch(instructions(9)){case Label(_, _) =>}
      //load 5
      assertMatch(instructions(10)){case MovRegImm(_, Imm(5), _) =>}
      val imm5 = instructions(10).asInstanceOf[MovRegImm].dest
      //load {a: 5}
      assertMatch(instructions(11)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -24), r, _) if r == imm5 =>}
      //save address of {a: 5}
      assertMatch(instructions(12)){case MovRegReg(_, RBP(X86RegMode.M64), _) =>}
      val rec = instructions(12).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(13)){case AddRegImm(r, Imm(-24), _) if r == rec =>}
      //x <- {a: 5}
      assertMatch(instructions(14)){case MovRegMem(_, MemOffset(r, 0), _) if r == rec =>}
      val workReg = instructions(14).asInstanceOf[MovRegMem].dest
      assertMatch(instructions(15)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -8), r, _) if r == workReg =>}
      //load x address
      assertMatch(instructions(16)){case MovRegReg(_, RBP(X86RegMode.M64), _) =>}
      val addrX = instructions(16).asInstanceOf[MovRegReg].dest
      assertMatch(instructions(17)){case AddRegImm(r, Imm(-8), _) if r == addrX =>}
      //y <- &x
      assertMatch(instructions(18)){case MovMemReg(MemOffset(RBP(X86RegMode.M64), -16), r, _) if r == addrX =>}
      //load y containing address of x
      assertMatch(instructions(19)){case MovRegMem(_, MemOffset(RBP(X86RegMode.M64), -16), _) =>}
      val loadY1 = instructions(19).asInstanceOf[MovRegMem].dest
      //load offset of "a"
      assertMatch(instructions(20)){case MovRegImm(_, Imm(0), _) =>}
      val offsetA1 = instructions(20).asInstanceOf[MovRegImm].dest
      //load address of (*y)
      assertMatch(instructions(21)){case MovRegReg(_, r, _) if r == loadY1 =>}
      val addrYa = instructions(21).asInstanceOf[MovRegReg].dest
      //add offset "a" to address of (*y)
      assertMatch(instructions(22)){case AddRegReg(r1, r2, _) if r1 == addrYa && r2 == offsetA1 =>}
      //load 8
      assertMatch(instructions(23)){case MovRegImm(_, Imm(8), _) =>}
      val imm8 = instructions(23).asInstanceOf[MovRegImm].dest
      //(*y).a <- 8
      assertMatch(instructions(24)){case MovMemReg(Mem(r1), r2, _) if r1 == addrYa && r2 == imm8 =>}
      //load y
      assertMatch(instructions(25)){case MovRegMem(_, MemOffset(RBP(X86RegMode.M64), -16), _) =>}
      val loadY2 = instructions(25).asInstanceOf[MovRegMem].dest
      //load *y
      assertMatch(instructions(26)){case MovRegReg(_, r, _) if r == loadY2 =>}
      val loadPY = instructions(26).asInstanceOf[MovRegReg].dest
      //load offset of "a"
      assertMatch(instructions(27)){case MovRegImm(_, Imm(0), _) =>}
      val offsetA2 = instructions(27).asInstanceOf[MovRegImm].dest
      //load address of (*y)
      assertMatch(instructions(28)){case MovRegReg(_, r, _) if r == loadPY =>}
      val loadPYa = instructions(28).asInstanceOf[MovRegReg].dest
      //add offset "a" to address of (*y)
      assertMatch(instructions(29)){case AddRegReg(r1, r2, _) if r1 == loadPYa && r2 == offsetA2 =>}
      //load (*y).a
      assertMatch(instructions(30)){case MovRegMem(_, Mem(r), _) if r == loadPYa =>}
      val loadYa = instructions(30).asInstanceOf[MovRegMem].dest
      //return x
      assertMatch(instructions(31)){case MovRegReg(RAX(X86RegMode.M32), r, _) if r == loadYa =>}
      assertMatch(instructions(32)){case MovRegReg(RSP(X86RegMode.M64), RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(33)){case PopReg(RBP(X86RegMode.M64), _) =>}
      assertMatch(instructions(34)){case Ret(_) =>}
    }

    test("Correct stack alignment of a stack frame") {
      val code =
        """
          |main() {
          |  var x, y, z;
          |  return 0;
          |}
          |""".stripMargin
      val instructions = codeToX86(code)

      //list of instructions should contain stack alignment and dealignment of 8 bytes (because stack size is 40)
      //32 = 3 variables (24 bytes) + 8 bytes alignment
      val frameAlloc = instructions.find {
        case SubRegImm(RSP(X86RegMode.M64), Imm(32), _) => true
        case _ => false
      }

      assert(frameAlloc.isDefined)
    }
    */
  }
}