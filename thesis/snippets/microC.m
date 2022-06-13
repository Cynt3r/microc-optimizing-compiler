Int  ::= 0 | 1 | -1 | 2 | -2 | ...
Id   ::= x | y | z | ...
Op   ::= + | - | * | / | > | ==
Exp  ::= Int
       | Id
       | Exp Op Exp
       | ( Exp )
       | Exp ( Exp, ... )
       | null
       | alloc Exp
       | & Id
       | * Exp
       | { Id : Exp, ... }
       | [ Exp, ... ]
       | Exp.Id
       | Exp[ Exp ]
       | input
Stmt ::= id = Exp;
       | * Exp = Exp;
       | Id.Id = Exp;
       | ( *Exp ).Id = Exp;
       | Exp[ Exp ] = Exp;
       | output Exp;
       | { Stmt ... }
       |
       | if ( Exp ) { Stmt } [ else { Stmt } ]
       | while ( Exp ) { Stmt }
Fun  ::= Id (Id, ...) {
            [ var Id, ... ; ]
            Stmt
            return Exp;
         }
Prog ::= Fun ...