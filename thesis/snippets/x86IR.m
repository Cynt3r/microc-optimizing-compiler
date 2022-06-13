Int   ::= 0 | 1 | -1 | 2 | -2 | ...
Uint  ::= 0 | 1 | 2 | ...
Label ::= x | y | z | ...
Loc   ::= Uint Uint
Op    ::= + | - | * | / | > | ==
Decl  ::= Label Loc
Type  ::= VoidType
        | SimpleType
        | ComposedType Int
        | PointType Type
Instr ::= Alloc Decl Type Loc
        | FunAlloc Decl Type Loc
        | HeapAlloc Instr Type Loc
        | ArgAddr Alloc Type Loc
        | GetAddr Instr Type Loc
        | GetAddroffset Instr Instr Type Loc
        | Load Instr Type Loc
        | LoadImm Int Type Loc
        | LoadInput Type Loc
        | LoadRecord Instr ... Type Loc
        | Store Instr Instr Type Loc
        | BinOp Op Instr Instr Type Loc
        | Call Instr Instr ... Type Loc
        | Print Instr Type Loc
        | Return Instr Type Loc
        | CondJump Instr Label Label Type Loc
        | Jump Label Type Loc
Block ::= Label Instr ...
Fun   ::= Label Int Block ...
x86IR ::= Fun ...