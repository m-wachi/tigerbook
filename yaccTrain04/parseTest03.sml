structure ParseTest03 = 
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
	     (* val (absyn, _) = Oldvb01Parser.parse(30,lexer,parseerror,()) *)
        in
            let
                  (*
                val (t1, s1) = Oldvb01Parser.Stream.get lexer
                val (t2, s2) = Oldvb01Parser.Stream.get s1
                  *)
                val ret = Oldvb01Parser.parse(0,lexer,parseerror,())
                val (ret1, ret2) = ret
            in
                TextIO.closeIn file;
                print ret1; print "\n";
                ret
            end
        end handle LrParser.ParseError => raise ErrorMsg.Error

end (* structure ParseTest03 *)
