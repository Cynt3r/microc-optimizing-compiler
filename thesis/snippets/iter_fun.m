//functions of a program
program.funs
    //basic blocks of a function
    .flatMap(_.bbs)
    //instructions of a basic block
    .flatMap(_.instructions)
    //filter non-terminating instructions
    .filter {
        case _: Terminator => false
        case _             => true
    }
    .foreach(i => println(i.name))