structure ParseTest02 = 
struct

  structure Stlnpg01LrVals =
    Stlnpg01LrValsFun(structure Token = LrParser.Token)

  structure Stlnpg01Lex =
    Stlnpg01LexFun(structure Tokens = Stlnpg01LrVals.Tokens)

  structure Stlnpg01Parser =
    Join(structure LrParser = LrParser
	 structure ParserData = Stlnpg01LrVals.ParserData
	 structure Lex = Stlnpg01Lex)


    fun parse filename =
        let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
            val file = TextIO.openIn filename
            fun get _ = TextIO.input file
	     fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
	     val lexer = LrParser.Stream.streamify (Stlnpg01Lex.makeLexer get)
	     (* val (absyn, _) = Stlnpg01Parser.parse(30,lexer,parseerror,()) *)
        in
            let
                  (*
                val (t1, s1) = Stlnpg01Parser.Stream.get lexer
                val (t2, s2) = Stlnpg01Parser.Stream.get s1
                  *)
                val ret = Stlnpg01Parser.parse(30,lexer,parseerror,())
            in
                TextIO.closeIn file;
                ret
            end
        end handle LrParser.ParseError => raise ErrorMsg.Error

end (* structure ParseTest02 *)
