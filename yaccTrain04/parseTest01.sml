structure ParseTest01 = 
struct

  structure Oldvb01LrVals =
    Oldvb01LrValsFun(structure Token = LrParser.Token)

  structure Oldvb01Lex =
    Oldvb01LexFun(structure Tokens = Oldvb01LrVals.Tokens)

  structure Oldvb01Parser =
    Join(structure LrParser = LrParser
	 structure ParserData = Oldvb01LrVals.ParserData
	 structure Lex = Oldvb01Lex)


    fun parse filename =
        let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
            val file = TextIO.openIn filename
            fun get _ = TextIO.input file
	     fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
	     val lexer = LrParser.Stream.streamify (Oldvb01Lex.makeLexer get)
        in
            let
                val (t1, s1) = Oldvb01Parser.Stream.get lexer
                val (t2, s2) = Oldvb01Parser.Stream.get s1
                val (t3, s3) = Oldvb01Parser.Stream.get s2
                val (t4, s4) = Oldvb01Parser.Stream.get s3
            in
                TextIO.closeIn file;
                (t1, t2, t3, t4)
            end
        end handle LrParser.ParseError => raise ErrorMsg.Error

end (* structure ParseTest01 *)
