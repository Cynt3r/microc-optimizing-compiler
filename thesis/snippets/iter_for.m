//functions of a program
for (fun <- program.funs) {
    //basic blocks of a function
    for (bb <- fun.bbs) {
        //instructions of a basic block
        for (i <- bb.instructions) {
            //filter non-terminating instructions
            if (!i.isInstanceOf[Terminator]) {
                println(i.name)
            }
        }
    }
}