structure lexTest =
struct 
    structure Stlnpg01LrVals =
        Stlnpg01LrValsFun(structure Token = LrParser.Token)

    structure Stlnpg01Lex =
        Stlnpg01LexFun(structure Tokens = Stlnpg01LrVals.Tokens)

    fun parse1 filename =
        let val file = TextIO.openIn filename
	     fun get _ = TextIO.input file
            val lexer = Stlnpg01Lex.makeLexer get
        in
             (* do_it(); *)
            let 
                val t1 = lexer()
                val t2 = lexer()
            in
	         TextIO.closeIn file;
                (t1, t2)
            end
        end
end

