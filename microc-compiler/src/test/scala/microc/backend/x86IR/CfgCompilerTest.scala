package microc.backend.x86IR

import microc.backend.BackendHelper
import microc.frontend.ast.{FunDecl, IdentifierDecl, Loc}
import utest._

object CfgCompilerTest extends TestSuite with BackendHelper {
  val tests: Tests = Tests {
    /*
    test("Basic CFG to x86IR compilation") {
      val ir = codeToIr(basicProgram)

      //function check
      ir.functions.size ==> 1
      val main = ir.functions.head
      main.name ==> "main"

      //basic block check
      main.bbs.size ==> 1
      val bb = main.bbs.head
      bb.name ==> "bb_1"

      //instructions check
      val actual = bb.instructions.toArray
      actual.length ==> 17
      actual(0) ==> Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7))
      actual(1) ==> Alloc(IdentifierDecl("y", Loc(3, 10)), PointType(SimpleType), Loc(3, 10))
      actual(2) ==> Alloc(IdentifierDecl("z", Loc(3, 13)), PointType(SimpleType), Loc(3, 13))
      actual(3) ==> LoadImm(3, SimpleType, Loc(4, 7))
      actual(4) ==> Store(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), LoadImm(3, SimpleType, Loc(4, 7)), Loc(4, 5))
      actual(5) ==> Load(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), Loc(5, 16))
      actual(6) ==> LoadImm(2, SimpleType, Loc(5, 12))
      actual(7) ==> BinOp(Plus, LoadImm(2, SimpleType, Loc(5, 12)), Load(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), Loc(5, 16)), SimpleType, Loc(5, 14))
      actual(8) ==> LoadImm(1, SimpleType, Loc(5, 7))
      actual(9) ==> BinOp(Plus, LoadImm(1, SimpleType, Loc(5, 7)), BinOp(Plus, LoadImm(2, SimpleType, Loc(5, 12)), Load(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), Loc(5, 16)), SimpleType, Loc(5, 14)), SimpleType, Loc(5, 9))
      actual(10) ==> Store(Alloc(IdentifierDecl("y", Loc(3, 10)), PointType(SimpleType), Loc(3, 10)), BinOp(Plus, LoadImm(1, SimpleType, Loc(5, 7)), BinOp(Plus, LoadImm(2, SimpleType, Loc(5, 12)), Load(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), Loc(5, 16)), SimpleType, Loc(5, 14)), SimpleType, Loc(5, 9)), Loc(5, 5))
      actual(11) ==> LoadImm(8, SimpleType, Loc(6, 11))
      actual(12) ==> Load(Alloc(IdentifierDecl("y", Loc(3, 10)), PointType(SimpleType), Loc(3, 10)), Loc(6, 7))
      actual(13) ==> BinOp(Times, Load(Alloc(IdentifierDecl("y", Loc(3, 10)), PointType(SimpleType), Loc(3, 10)), Loc(6, 7)), LoadImm(8, SimpleType, Loc(6, 11)), SimpleType, Loc(6, 9))
      actual(14) ==> Store(Alloc(IdentifierDecl("z", Loc(3, 13)), PointType(SimpleType), Loc(3, 13)), BinOp(Times, Load(Alloc(IdentifierDecl("y", Loc(3, 10)), PointType(SimpleType), Loc(3, 10)), Loc(6, 7)), LoadImm(8, SimpleType, Loc(6, 11)), SimpleType, Loc(6, 9)), Loc(6, 5))
      actual(15) ==> Load(Alloc(IdentifierDecl("z", Loc(3, 13)), PointType(SimpleType), Loc(3, 13)), Loc(7, 10))
      actual(16) ==> Return(Load(Alloc(IdentifierDecl("z", Loc(3, 13)), PointType(SimpleType), Loc(3, 13)), Loc(7, 10)), Loc(7, 3))
    }

    test("CFG with if statement to x86IR compilation") {
      val ir = codeToIr(ifProgram)

      //function check
      ir.functions.size ==> 1
      val main = ir.functions.head
      main.name ==> "main"

      //basic block check
      val bbs = main.bbs.toArray
      bbs.length ==> 4
      bbs(0).name ==> "bb_1"
      bbs(1).name ==> "then_2"
      bbs(2).name ==> "else_2"
      bbs(3).name ==> "finally_2"

      //instructions check
      //bb_1
      val actualBb1 = bbs(0).instructions.toArray
      actualBb1.length ==> 3
      actualBb1(0) ==> Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7))
      actualBb1(1) ==> LoadImm(1, SimpleType, Loc(4, 7))
      actualBb1(2) ==> CondJump(LoadImm(1, SimpleType, Loc(4, 7)), "then_2", "else_2", Loc(4, 7))

      //then_2
      val actualThen = bbs(1).instructions.toArray
      actualThen.length ==> 3
      actualThen(0) ==> LoadImm(42, SimpleType, Loc(5, 9))
      actualThen(1) ==> Store(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), LoadImm(42, SimpleType, Loc(5, 9)), Loc(5, 7))
      actualThen(2) ==> Jump("finally_2", Loc(5, 7))

      //else_2
      val actualElse = bbs(2).instructions.toArray
      actualElse.length ==> 3
      actualElse(0) ==> LoadImm(-42, SimpleType, Loc(7, 9))
      actualElse(1) ==> Store(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), LoadImm(-42, SimpleType, Loc(7, 9)), Loc(7, 7))
      actualElse(2) ==> Jump("finally_2", Loc(7, 7))

      //finally_2
      val actualFinally = bbs(3).instructions.toArray
      actualFinally.length ==> 2
      actualFinally(0) ==> Load(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), Loc(9, 10))
      actualFinally(1) ==> Return(Load(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), Loc(9, 10)), Loc(9, 3))
    }

    test("CFG with while statement to x86IR compilation") {
      val ir = codeToIr(whileProgram)

      //function check
      ir.functions.size ==> 1
      val main = ir.functions.head
      main.name ==> "main"

      //basic block check
      val bbs = main.bbs.toArray
      bbs.length ==> 4
      bbs(0).name ==> "bb_1"
      bbs(1).name ==> "guard_2"
      bbs(2).name ==> "body_2"
      bbs(3).name ==> "finally_2"

      //instructions check
      //bb_1
      val actualBb1 = bbs(0).instructions.toArray
      actualBb1.length ==> 2
      actualBb1(0) ==> Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7))
      actualBb1(1) ==> Jump("guard_2", Loc(4, 10))

      //while_guard_2
      val actualGuard = bbs(1).instructions.toArray
      actualGuard.length ==> 2
      actualGuard(0) ==> LoadImm(1, SimpleType, Loc(4, 10))
      actualGuard(1) ==> CondJump(LoadImm(1, SimpleType, Loc(4, 10)), "body_2", "finally_2", Loc(4, 10))

      //while_body_2
      val actualBody = bbs(2).instructions.toArray
      actualBody.length ==> 3
      actualBody(0) ==> LoadImm(42, SimpleType, Loc(5, 9))
      actualBody(1) ==> Store(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), LoadImm(42, SimpleType, Loc(5, 9)), Loc(5, 7))
      actualBody(2) ==> Jump("guard_2", Loc(5, 7))

      //while_after_2
      val actualAfter = bbs(3).instructions.toArray
      actualAfter.length ==> 2
      actualAfter(0) ==> Load(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), Loc(7, 10))
      actualAfter(1) ==> Return(Load(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), Loc(7, 10)), Loc(7, 3))
    }

    test("CFG with function call to x86IR compilation") {
      val ir = codeToIr(funProgram)

      //function check
      ir.functions.size ==> 2
      val foo = ir.functions.head
      val main = ir.functions.tail.head

      //foo
      foo.name ==> "foo"
      foo.bbs.size ==> 1
      val fooBb = foo.bbs.head.instructions.toArray
      fooBb.length ==> 9
      fooBb(0) ==> ArgAddr(Alloc(IdentifierDecl("a", Loc(2, 5)), PointType(SimpleType), Loc(2, 5)), Loc(2, 5))
      fooBb(1) ==> ArgAddr(Alloc(IdentifierDecl("b", Loc(2, 8)), PointType(SimpleType), Loc(2, 8)), Loc(2, 8))
      fooBb(2) ==> Alloc(IdentifierDecl("c", Loc(3, 7)), PointType(SimpleType), Loc(3, 7))
      fooBb(3) ==> Load(Alloc(IdentifierDecl("b", Loc(2, 8)), PointType(SimpleType), Loc(2, 8)), Loc(4, 11))
      fooBb(4) ==> Load(Alloc(IdentifierDecl("a", Loc(2, 5)), PointType(SimpleType), Loc(2, 5)), Loc(4, 7))
      fooBb(5) ==> BinOp(Plus, Load(Alloc(IdentifierDecl("a", Loc(2, 5)), PointType(SimpleType), Loc(2, 5)), Loc(4, 7)), Load(Alloc(IdentifierDecl("b", Loc(2, 8)), PointType(SimpleType), Loc(2, 8)), Loc(4, 11)), SimpleType, Loc(4, 9))
      fooBb(6) ==> Store(Alloc(IdentifierDecl("c", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), BinOp(Plus, Load(Alloc(IdentifierDecl("a", Loc(2, 5)), PointType(SimpleType), Loc(2, 5)), Loc(4, 7)), Load(Alloc(IdentifierDecl("b", Loc(2, 8)), PointType(SimpleType), Loc(2, 8)), Loc(4, 11)), SimpleType, Loc(4, 9)), Loc(4, 5))
      fooBb(7) ==> Load(Alloc(IdentifierDecl("c", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), Loc(5, 10))
      fooBb(8) ==> Return(Load(Alloc(IdentifierDecl("c", Loc(3, 7)), PointType(SimpleType), Loc(3, 7)), Loc(5, 10)), Loc(5, 3))

      //main
      main.name ==> "main"
      main.bbs.size ==> 1
      val mainBb = main.bbs.head.instructions.toArray
      mainBb.length ==> 8
      mainBb(0) ==> Alloc(IdentifierDecl("x", Loc(9, 7)), PointType(SimpleType), Loc(9, 7))
      assertMatch(mainBb(1)){case Load(FunAlloc(FunDecl("foo", _, _, _), _), _) =>}
      val fun = mainBb(1)
      mainBb(2) ==> LoadImm(19, SimpleType, Loc(10, 11))
      mainBb(3) ==> LoadImm(23, SimpleType, Loc(10, 15))
      mainBb(4) ==> Call(fun, List(LoadImm(19, SimpleType, Loc(10, 11)), LoadImm(23, SimpleType, Loc(10, 15))), SimpleType, Loc(10, 10))
      mainBb(5) ==> Store(Alloc(IdentifierDecl("x", Loc(9, 7)), PointType(SimpleType), Loc(9, 7)), Call(fun, List(LoadImm(19, SimpleType, Loc(10, 11)), LoadImm(23, SimpleType, Loc(10, 15))), SimpleType, Loc(10, 10)), Loc(10, 5))
      mainBb(6) ==> Load(Alloc(IdentifierDecl("x", Loc(9, 7)), PointType(SimpleType), Loc(9, 7)), Loc(11, 10))
      mainBb(7) ==> Return(Load(Alloc(IdentifierDecl("x", Loc(9, 7)), PointType(SimpleType), Loc(9, 7)), Loc(11, 10)), Loc(11, 3))
    }

    test("CFG with records to x86IR compilation") {
      val ir = codeToIr(recProgram)

      //function check
      ir.functions.size ==> 1
      val main = ir.functions.head
      main.name ==> "main"

      //basic block check
      main.bbs.length ==> 1
      val bb = main.bbs.head
      bb.name ==> "bb_1"

      //instructions check
      val actual = bb.instructions.toArray
      actual.length ==> 19
      actual(0) ==> Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(24)), Loc(3, 7))
      actual(1) ==> Alloc(IdentifierDecl("y", Loc(3, 10)), PointType(ComposedType(16)), Loc(3, 10))
      actual(2) ==> LoadImm(8, SimpleType, Loc(4, 15))
      actual(3) ==> LoadImm(9, SimpleType, Loc(4, 21))
      actual(4) ==> LoadComposed(List(LoadImm(8, SimpleType, Loc(4, 15)), LoadImm(9, SimpleType, Loc(4, 21))), ComposedType(16), Loc(4, 11))
      val subRec = actual(4)
      actual(5) ==> LoadImm(5, SimpleType, Loc(4, 28))
      actual(6) ==> LoadComposed(List(subRec, LoadImm(5, SimpleType, Loc(4, 28))), ComposedType(24), Loc(4, 7))
      val rec = actual(6)
      actual(7) ==> Store(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(24)), Loc(3, 7)), rec, Loc(4, 5))
      actual(8) ==> LoadImm(-16, PointType(SimpleType), Loc(5, 4))
      actual(9) ==> GetAddrOffset(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(24)), Loc(3, 7)), LoadImm(-16, PointType(SimpleType), Loc(5, 4)), PointType(SimpleType), Loc(5, 4))
      actual(10) ==> LoadImm(42, SimpleType, Loc(5, 9))
      actual(11) ==> Store(GetAddrOffset(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(24)), Loc(3, 7)), LoadImm(-16, PointType(SimpleType), Loc(5, 4)), PointType(SimpleType), Loc(5, 4)), LoadImm(42, SimpleType, Loc(5, 9)), Loc(5, 7))
      actual(12) ==> Load(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(24)), Loc(3, 7)), Loc(6, 7))
      actual(13) ==> LoadImm(0, PointType(ComposedType(16)), Loc(6, 8))
      actual(14) ==> GetAddrOffset(Load(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(24)), Loc(3, 7)), Loc(6, 7)), LoadImm(0, PointType(ComposedType(16)), Loc(6, 8)), PointType(ComposedType(16)), Loc(6, 8))
      val addr = actual(14)
      actual(15) ==> Load(addr, Loc(6, 8))
      actual(16) ==> Store(Alloc(IdentifierDecl("y", Loc(3, 10)), PointType(ComposedType(16)), Loc(3, 10)), Load(addr, Loc(6, 8)), Loc(6, 5))
      actual(17) ==> LoadImm(0, SimpleType, Loc(7, 10))
      actual(18) ==> Return(LoadImm(0, SimpleType, Loc(7, 10)), Loc(7, 3))
    }

    test("CFG with function calls using records to x86IR compilation") {
      val ir = codeToIr(funRecProgram)

      //function check
      ir.functions.size ==> 2
      val foo = ir.functions.head
      val main = ir.functions.tail.head

      //foo
      foo.name ==> "foo"
      foo.bbs.size ==> 1
      val fooBb = foo.bbs.head.instructions.toArray
      fooBb.length ==> 11
      fooBb(0) ==> ArgAddr(Alloc(IdentifierDecl("rec", Loc(2, 5)), PointType(ComposedType(16)), Loc(2, 5)), Loc(2, 5))
      fooBb(1) ==> Load(Alloc(IdentifierDecl("rec", Loc(2, 5)), PointType(ComposedType(16)), Loc(2, 5)), Loc(3, 14))
      fooBb(2) ==> LoadImm(-8, PointType(SimpleType), Loc(3, 17))
      fooBb(3) ==> GetAddrOffset(Load(Alloc(IdentifierDecl("rec", Loc(2, 5)), PointType(ComposedType(16)), Loc(2, 5)), Loc(3, 14)), LoadImm(-8, PointType(SimpleType), Loc(3, 17)), PointType(SimpleType), Loc(3, 17))
      val addr1 = fooBb(3)
      fooBb(4) ==> Load(addr1, Loc(3, 17))
      fooBb(5) ==> Load(Alloc(IdentifierDecl("rec", Loc(2, 5)), PointType(ComposedType(16)), Loc(2, 5)), Loc(3, 24))
      fooBb(6) ==> LoadImm(0, PointType(SimpleType), Loc(3, 27))
      fooBb(7) ==> GetAddrOffset(Load(Alloc(IdentifierDecl("rec", Loc(2, 5)), PointType(ComposedType(16)), Loc(2, 5)), Loc(3, 24)), LoadImm(0, PointType(SimpleType), Loc(3, 27)), PointType(SimpleType), Loc(3, 27))
      val addr2 = fooBb(7)
      fooBb(8) ==> Load(addr2, Loc(3, 27))
      fooBb(9) ==> LoadComposed(List(Load(addr1, Loc(3, 17)), Load(addr2, Loc(3, 27))), ComposedType(16), Loc(3, 10))
      val rec = fooBb(9)
      fooBb(10) ==> Return(rec, Loc(3, 3))

      //main
      main.name ==> "main"
      main.bbs.size ==> 1
      val mainBb = main.bbs.head.instructions.toArray
      mainBb.length ==> 11
      mainBb(0) ==> Alloc(IdentifierDecl("x", Loc(7, 7)), PointType(ComposedType(16)), Loc(7, 7))
      mainBb(1) ==> LoadImm(1, SimpleType, Loc(8, 11))
      mainBb(2) ==> LoadImm(2, SimpleType, Loc(8, 17))
      mainBb(3) ==> LoadComposed(List(LoadImm(1, SimpleType, Loc(8, 11)), LoadImm(2, SimpleType, Loc(8, 17))), ComposedType(16), Loc(8, 7))
      mainBb(4) ==> Store(Alloc(IdentifierDecl("x", Loc(7, 7)), PointType(ComposedType(16)), Loc(7, 7)), LoadComposed(List(LoadImm(1, SimpleType, Loc(8, 11)), LoadImm(2, SimpleType, Loc(8, 17))), ComposedType(16), Loc(8, 7)), Loc(8, 5))
      assertMatch(mainBb(5)){case Load(FunAlloc(FunDecl("foo", _, _, _), _), _) =>}
      val fun = mainBb(5)
      mainBb(6) ==> Load(Alloc(IdentifierDecl("x", Loc(7, 7)), PointType(ComposedType(16)), Loc(7, 7)), Loc(9, 11))
      mainBb(7) ==> Call(fun, List(Load(Alloc(IdentifierDecl("x", Loc(7, 7)), PointType(ComposedType(16)), Loc(7, 7)), Loc(9, 11))), ComposedType(16), Loc(9, 10))
      val call = mainBb(7)
      mainBb(8) ==> Store(Alloc(IdentifierDecl("x", Loc(7, 7)), PointType(ComposedType(16)), Loc(7, 7)), call, Loc(9, 5))
      mainBb(9) ==> LoadImm(0, SimpleType, Loc(10, 10))
      mainBb(10) ==> Return(LoadImm(0, SimpleType, Loc(10, 10)), Loc(10, 3))
    }

    test("CFG with pointers to x86IR compilation") {
      val ir = codeToIr(pointProgram)

      //function check
      ir.functions.size ==> 1
      val main = ir.functions.head
      main.name ==> "main"

      //basic block check
      main.bbs.length ==> 1
      val bb = main.bbs.head
      bb.name ==> "bb_1"

      //instructions check
      val actual = bb.instructions.toArray
      actual.length ==> 16
      actual(0) ==> Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(SimpleType), Loc(3, 7))
      val allocX = actual(0)
      actual(1) ==> Alloc(IdentifierDecl("y", Loc(3, 10)), PointType(PointType(SimpleType)), Loc(3, 10))
      val allocY = actual(1)
      actual(2) ==> LoadImm(5, SimpleType, Loc(4, 7))
      actual(3) ==> Store(allocX, LoadImm(5, SimpleType, Loc(4, 7)), Loc(4, 5))
      actual(4) ==> GetAddr(allocX, Loc(5, 7))
      actual(5) ==> Store(allocY, GetAddr(allocX, Loc(5, 7)), Loc(5, 5))
      actual(6) ==> Load(allocY, Loc(6, 3))
      actual(7) ==> LoadImm(8, SimpleType, Loc(6, 8))
      actual(8) ==> Store(Load(allocY, Loc(6, 3)), LoadImm(8, SimpleType, Loc(6, 8)), Loc(6, 6))
      actual(9) ==> Load(allocY, Loc(7, 8))
      actual(10) ==> Load(Load(allocY, Loc(7, 8)), Loc(7, 7))
      val loadPY = actual(10)
      actual(11) ==> Store(allocX, loadPY, Loc(7, 5))
      actual(12) ==> LoadImm(0, PointType(SimpleType), Loc(8, 7))
      actual(13) ==> Store(allocY, LoadImm(0, PointType(SimpleType), Loc(8, 7)), Loc(8, 5))
      actual(14) ==> LoadImm(0, SimpleType, Loc(9, 10))
      actual(15) ==> Return(LoadImm(0, SimpleType, Loc(9, 10)), Loc(9, 3))
    }

    test("CFG with record pointers to x86IR compilation") {
      val ir = codeToIr(pointRecProgram)

      //function check
      ir.functions.size ==> 1
      val main = ir.functions.head
      main.name ==> "main"

      //basic block check
      main.bbs.length ==> 1
      val bb = main.bbs.head
      bb.name ==> "bb_1"

      //instructions check
      val actual = bb.instructions.toArray
      actual.length ==> 18
      actual(0) ==> Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(8)), Loc(3, 7))
      val allocX = actual(0)
      actual(1) ==> Alloc(IdentifierDecl("y", Loc(3, 10)), PointType(PointType(ComposedType(8))), Loc(3, 10))
      val allocY = actual(1)
      actual(2) ==> LoadImm(5, SimpleType, Loc(4, 11))
      actual(3) ==> LoadComposed(List(LoadImm(5, SimpleType, Loc(4, 11))), ComposedType(8), Loc(4, 7))
      actual(4) ==> Store(allocX, LoadComposed(List(LoadImm(5, SimpleType, Loc(4, 11))), ComposedType(8), Loc(4, 7)), Loc(4, 5))
      actual(5) ==> GetAddr(allocX, Loc(5, 7))
      val loadPX = actual(5)
      actual(6) ==> Store(allocY, loadPX, Loc(5, 5))
      actual(7) ==> Load(allocY, Loc(6, 4))
      actual(8) ==> LoadImm(0, PointType(SimpleType), Loc(6, 7))
      actual(9) ==> GetAddrOffset(Load(allocY, Loc(6, 4)), LoadImm(0, PointType(SimpleType), Loc(6, 7)), PointType(SimpleType), Loc(6, 7))
      val addr1 = actual(9)
      actual(10) ==> LoadImm(8, SimpleType, Loc(6, 12))
      actual(11) ==> Store(addr1, LoadImm(8, SimpleType, Loc(6, 12)), Loc(6, 10))
      actual(12) ==> Load(allocY, Loc(7, 12))
      actual(13) ==> Load(Load(allocY, Loc(7, 12)), Loc(7, 11))
      actual(14) ==> LoadImm(0, PointType(SimpleType), Loc(7, 14))
      actual(15) ==> GetAddrOffset(Load(Load(allocY, Loc(7, 12)), Loc(7, 11)), LoadImm(0, PointType(SimpleType), Loc(7, 14)), PointType(SimpleType), Loc(7, 14))
      val addr2 = actual(15)
      actual(16) ==> Load(addr2, Loc(7, 14))
      actual(17) ==> Return(Load(addr2, Loc(7, 14)), Loc(7, 3))
    }

    test("CFG with do-while statement to x86IR compilation") {
      val ir = new CfgCompiler(doWhileCfg(), Map(), Map()).compile()

      //function check
      ir.functions.size ==> 1
      val main = ir.functions.head
      main.name ==> "main"

      //basic block check
      val bbs = main.bbs.toArray
      bbs.length ==> 4
      bbs(0).name ==> "bb_1"
      bbs(1).name ==> "body_2"
      bbs(2).name ==> "guard_2"
      bbs(3).name ==> "finally_2"

      //instructions check
      //bb_1
      val actualBb1 = bbs(0).instructions.toArray
      actualBb1.length ==> 1
      actualBb1(0) ==> Jump("body_2", Loc(0, 0))

      //body_2
      val actualBody = bbs(1).instructions.toArray
      actualBody.length ==> 3
      actualBody(0) ==> LoadImm(1, SimpleType, Loc(0, 0))
      actualBody(1) ==> Print(LoadImm(1, SimpleType, Loc(0, 0)), Loc(0, 0))
      actualBody(2) ==> Jump("guard_2", Loc(0, 0))

      //guard_2
      val actualGuard = bbs(2).instructions.toArray
      actualGuard.length ==> 2
      actualGuard(0) ==> LoadImm(0, SimpleType, Loc(0, 0))
      actualGuard(1) ==> CondJump(LoadImm(0, SimpleType, Loc(0, 0)), "body_2", "finally_2", Loc(0, 0))

      //finally_2
      val actualFinally = bbs(3).instructions.toArray
      actualFinally.length ==> 2
      actualFinally(0) ==> LoadImm(3, SimpleType, Loc(0, 0))
      actualFinally(1) ==> Return(LoadImm(3, SimpleType, Loc(0, 0)), Loc(0, 0))
    }

    test("CFG with arrays to x86IR compilation") {
      val ir = codeToIr(arrProgram)

      //function check
      ir.functions.size ==> 1
      val main = ir.functions.head
      main.name ==> "main"

      //basic block check
      main.bbs.size ==> 1
      val bb = main.bbs.head
      bb.name ==> "bb_1"

      //instructions check
      val actual = bb.instructions.toArray
      actual.length ==> 24
      actual(0) ==> Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(24)), Loc(3, 7))
      actual(1) ==> Alloc(IdentifierDecl("y", Loc(3, 10)), PointType(ComposedType(24)), Loc(3, 10))
      actual(2) ==> LoadImm(1, SimpleType, Loc(4, 8))
      actual(3) ==> LoadImm(2, SimpleType, Loc(4, 11))
      actual(4) ==> LoadImm(3, SimpleType, Loc(4, 14))
      actual(5) ==> LoadComposed(List(LoadImm(1, SimpleType, Loc(4, 8)), LoadImm(2, SimpleType, Loc(4, 11)), LoadImm(3, SimpleType, Loc(4, 14))), ComposedType(24), Loc(4, 7))
      val arr = actual(5)
      actual(6) ==> Store(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(24)), Loc(3, 7)), arr, Loc(4, 5))
      actual(7) ==> Load(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(24)), Loc(3, 7)), Loc(5, 7))
      actual(8) ==> Store(Alloc(IdentifierDecl("y", Loc(3, 10)), PointType(ComposedType(24)), Loc(3, 10)), Load(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(24)), Loc(3, 7)), Loc(5, 7)), Loc(5, 5))
      actual(9) ==> LoadImm(2, SimpleType, Loc(6, 5))
      actual(10) ==> LoadImm(-8, PointType(SimpleType), Loc(6, 4))
      actual(11) ==> BinOp(Times, LoadImm(2, SimpleType, Loc(6, 5)), LoadImm(-8, PointType(SimpleType), Loc(6, 4)), PointType(SimpleType), Loc(6, 4))
      val offset1 = actual(11)
      actual(12) ==> GetAddrOffset(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(24)), Loc(3, 7)), offset1, PointType(SimpleType), Loc(6, 4))
      val addr1 = actual(12)
      actual(13) ==> LoadImm(42, SimpleType, Loc(6, 10))
      actual(14) ==> Store(addr1, LoadImm(42, SimpleType, Loc(6, 10)), Loc(6, 8))
      actual(15) ==> Load(Alloc(IdentifierDecl("y", Loc(3, 10)), PointType(ComposedType(24)), Loc(3, 10)), Loc(7, 10))
      val loadY = actual(15)
      actual(16) ==> LoadImm(2, SimpleType, Loc(7, 16))
      actual(17) ==> LoadImm(4, SimpleType, Loc(7, 12))
      actual(18) ==> BinOp(Divide, LoadImm(4, SimpleType, Loc(7, 12)), LoadImm(2, SimpleType, Loc(7, 16)), SimpleType, Loc(7, 14))
      val index = actual(18)
      actual(19) ==> LoadImm(-8, PointType(SimpleType), Loc(7, 11))
      actual(20) ==> BinOp(Times, index, LoadImm(-8, PointType(SimpleType), Loc(7, 11)), PointType(SimpleType), Loc(7, 11))
      val offset2 = actual(20)
      actual(21) ==> GetAddrOffset(loadY, offset2, PointType(SimpleType), Loc(7, 11))
      val addr2 = actual(21)
      actual(22) ==> Load(addr2, Loc(7, 11))
      actual(23) ==> Return(Load(addr2, Loc(7, 11)), Loc(7, 3))
    }

    test("CFG with array pointers to x86IR compilation") {
      val ir = codeToIr(pointArrProgram)

      //function check
      ir.functions.size ==> 1
      val main = ir.functions.head
      main.name ==> "main"

      //basic block check
      main.bbs.size ==> 1
      val bb = main.bbs.head
      bb.name ==> "bb_1"

      //instructions check
      val actual = bb.instructions.toArray
      actual.length ==> 24
      actual(0) ==> Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(24)), Loc(3, 7))
      val allocX = actual(0)
      actual(1) ==> Alloc(IdentifierDecl("y", Loc(3, 10)), PointType(PointType(ComposedType(24))), Loc(3, 10))
      val allocY = actual(1)
      actual(2) ==> LoadImm(1, SimpleType, Loc(4, 8))
      actual(3) ==> LoadImm(2, SimpleType, Loc(4, 11))
      actual(4) ==> LoadImm(3, SimpleType, Loc(4, 14))
      actual(5) ==> LoadComposed(List(LoadImm(1, SimpleType, Loc(4, 8)), LoadImm(2, SimpleType, Loc(4, 11)), LoadImm(3, SimpleType, Loc(4, 14))), ComposedType(24), Loc(4, 7))
      val arr = actual(5)
      actual(6) ==> Store(Alloc(IdentifierDecl("x", Loc(3, 7)), PointType(ComposedType(24)), Loc(3, 7)), arr, Loc(4, 5))
      actual(7) ==> GetAddr(allocX, Loc(5, 7))
      actual(8) ==> Store(allocY, GetAddr(allocX, Loc(5, 7)), Loc(5, 5))
      actual(9) ==> Load(allocY, Loc(6, 4))
      actual(10) ==> LoadImm(0, SimpleType, Loc(6, 8))
      actual(11) ==> LoadImm(-8, PointType(SimpleType), Loc(6, 7))
      actual(12) ==> BinOp(Times, LoadImm(0, SimpleType, Loc(6, 8)), LoadImm(-8, PointType(SimpleType), Loc(6, 7)), PointType(SimpleType), Loc(6, 7))
      val offset1 = actual(12)
      actual(13) ==> GetAddrOffset(Load(allocY, Loc(6, 4)), offset1, PointType(SimpleType), Loc(6, 7))
      val addr1 = actual(13)
      actual(14) ==> LoadImm(42, SimpleType, Loc(6, 13))
      actual(15) ==> Store(addr1, LoadImm(42, SimpleType, Loc(6, 13)), Loc(6, 11))
      actual(16) ==> Load(allocY, Loc(7, 12))
      actual(17) ==> Load(Load(allocY, Loc(7, 12)), Loc(7, 11))
      actual(18) ==> LoadImm(0, SimpleType, Loc(7, 15))
      actual(19) ==> LoadImm(-8, PointType(SimpleType), Loc(7, 14))
      actual(20) ==> BinOp(Times, LoadImm(0, SimpleType, Loc(7, 15)), LoadImm(-8, PointType(SimpleType), Loc(7, 14)), PointType(SimpleType), Loc(7, 14))
      val offset2 = actual(20)
      actual(21) ==> GetAddrOffset(Load(Load(allocY, Loc(7, 12)), Loc(7, 11)), offset2, PointType(SimpleType), Loc(7, 14))
      val addr2 = actual(21)
      actual(22) ==> Load(addr2, Loc(7, 14))
      actual(23) ==> Return(Load(addr2, Loc(7, 14)), Loc(7, 3))
    }
    */
  }
}
