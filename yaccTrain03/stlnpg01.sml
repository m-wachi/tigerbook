structure Stlnpg01 : sig
    val parse : unit -> unit
end =
struct

  structure Stlnpg01LrVals =
    Stlnpg01LrValsFun(structure Token = LrParser.Token)

  structure Stlnpg01Lex =
    Stlnpg01LexFun(structure Tokens = Stlnpg01LrVals.Tokens)

  structure Stlnpg01Parser =
    Join(structure LrParser = LrParser
	 structure ParserData = Stlnpg01LrVals.ParserData
	 structure Lex = Stlnpg01Lex)

  fun invoke lexstream =
      let fun print_error (s,i:int,_) =
	      TextIO.output(TextIO.stdOut,
			    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
       in Stlnpg01Parser.parse(0,lexstream,print_error,())
      end

(* 
 * Finally, we need a driver function that reads one or more expressions
 * from the standard input. The function parse, shown below, does
 * this. It runs the calculator on the standard input and terminates when
 * an end-of-file is encountered.
 *)

  fun parse () = 
      let val lexer = Stlnpg01Parser.makeLexer (fn _ =>
                                               (case TextIO.inputLine TextIO.stdIn
                                                of SOME s => s
                                                 | _ => ""))
	  val dummyEOF = Stlnpg01LrVals.Tokens.EOF(0,0)
	  val dummySEMI = Stlnpg01LrVals.Tokens.SEMICOLON(0,0)
	  fun loop lexer =
	      let val (result,lexer) = invoke lexer
		  val (nextToken,lexer) = Stlnpg01Parser.Stream.get lexer
                  (*
		  val _ = case result
			    of SOME r =>
				TextIO.output(TextIO.stdOut,
				       "result = " ^ (Int.toString r) ^ "\n")
			     | NONE => ()
                  *)
	       in if Stlnpg01Parser.sameToken(nextToken,dummyEOF) then ()
		  else loop lexer
	      end
       in loop lexer
      end

end (* structure Stlnpg01 *)
