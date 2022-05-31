structure Oldvb01 = 
struct

  structure Oldvb01LrVals =
    Oldvb01LrValsFun(structure Token = LrParser.Token)

  structure Oldvb01Lex =
    Oldvb01LexFun(structure Tokens = Oldvb01LrVals.Tokens)

  structure Oldvb01Parser =
    Join(structure LrParser = LrParser
	 structure ParserData = Oldvb01LrVals.ParserData
	 structure Lex = Oldvb01Lex)

    fun doLex filename =
        let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
            val file = TextIO.openIn filename
            fun get _ = TextIO.input file
	     fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
	     val lexer = LrParser.Stream.streamify (Oldvb01Lex.makeLexer get)
            val dummyEOF = Oldvb01LrVals.Tokens.EOF(0,0)
            val dummyINT = Oldvb01LrVals.Tokens.INT(0, 0, 0)
            val dummySTR = Oldvb01LrVals.Tokens.STRING("STR", 0, 0)
	     fun loop lexer tokens =
	         let val (result, lexer) = Oldvb01Parser.Stream.get lexer
		  in
                    print "get token.\n";
     	             if Oldvb01Parser.sameToken(result, dummyEOF)
                    then tokens
		      else loop lexer (result :: tokens)
		 end
        in
            let
                val a = loop lexer nil
                val b = rev a
                val c = hd a
                (* val (_, d) = case c of Oldvb01Parser.Token.Token (g, h) => (g, h) *)

            in
                TextIO.closeIn file;
                if Oldvb01Parser.sameToken(c, dummyINT)
                then print "INT Token\n" else ();
		  b
            end
        end handle LrParser.ParseError => raise ErrorMsg.Error

    fun parse filename =
        let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
            val file = TextIO.openIn filename
            fun get _ = TextIO.input file
	     fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
	     val lexer = LrParser.Stream.streamify (Oldvb01Lex.makeLexer get)
	     (* val (absyn, _) = Oldvb01Parser.parse(30,lexer,parseerror,()) *)
        in
            let
                val ret = Oldvb01Parser.parse(0,lexer,parseerror,())
            in
                TextIO.closeIn file;
                (* print ret1; print "\n"; *)
                ret
            end
        end handle LrParser.ParseError => raise ErrorMsg.Error

    fun symToStr (sym: Symbol.symbol) =
        Symbol.name sym

    fun varToStr (v: Absyn.var) =
        case v of
	    Absyn.SimpleVar (sym, _) => symToStr sym

    fun stmtToStr (stmt: Absyn.statement) =
        case stmt of
	    Absyn.LclVarDecl (v, t) =>
                "LclVarDecl var=" ^ (varToStr v)
            | _ => "unexpected stmt. stmtToStr."


    fun test01 p1 =
        let
	    val (b, _) = p1
	    val c = hd b
	in
	    c
	end


    fun test02 (p1, p2) =
        stmtToStr p1

end (* structure Oldvb01 *)
