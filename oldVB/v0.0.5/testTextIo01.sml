fun runTest01 () =
    let 
        val os = TextIO.openOut "testout01.txt"
    in
        TextIO.output (os, "Hello world.\n");
        TextIO.output (os, "I am Testprogram.\n");
        TextIO.output (os, "Nice to meet you.\n");
        TextIO.closeOut os
    end
