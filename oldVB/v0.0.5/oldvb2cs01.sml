structure Oldvb2Cs01 = 
struct

    fun convProcName (procName: string) : string =     
        case procName of
            "Msgbox" => "Console.WriteLine"
            | _ => procName

    fun convSym (sym: Symbol.symbol) = Symbol.name sym

    fun convVbType (t: Absyn.vbtype) =
        case t of
            Absyn.VbTyString => "string"
            | Absyn.VbTyInt => "int"

    fun convVar (v: Absyn.var) =
        case v of
            Absyn.SimpleVar (sym, _) => convSym sym
     
    fun convExp (e: Absyn.exp) =
        case e of
            Absyn.VarExp v => convVar v
            | Absyn.IntExp i => Int.toString i
            | Absyn.StringExp (s, _) => "\"" ^ s ^ "\""

    fun convProcParams (params: Absyn.exp list) =
        if null params then ""
        else
            let
                val p::ps = params
            in
                (convExp p) ^ "," ^ (convProcParams ps)
            end

     
    fun convStmt (stmt: Absyn.statement) =
        case stmt of
            Absyn.LclVarDecl (v, t) =>
                (convVbType t) ^ " " ^ (convVar v) ^ ";"
            | Absyn.BlankLine => ""
            | Absyn.AssignStmt (v, e) => (convVar v) ^ " = " ^ (convExp e) ^ ";"
            | Absyn.DefProc (sym, lines) =>
                let
	             val procName = convSym sym
                    val procHdr = "static void " ^ procName ^ "()\n{\n" 
                    val body = convLglines lines
                in
                    procHdr ^ body ^ "}"
                end
            | Absyn.CallProc (sym, params) =>
                let
                    val procName = convProcName (convSym sym)
                in
                     procName ^ "(" ^ (convProcParams params) ^ ");"
                end

    and convLgline (stmt: Absyn.statement, cmnt: Absyn.comment) =
        let 
            val sStmt = convStmt stmt
        in
            if "" = cmnt then sStmt ^ "\n"
            else sStmt ^ "//" ^ cmnt ^ "\n"
        end    

    and convLglines (lines: Absyn.lgline list) =
        if null lines then ""
        else
            let 
                val ln::lns = lines
            in
                (convLgline ln) ^ (convLglines lns)
            end

    fun convAll (os: TextIO.outstream, ast) =
        TextIO.output (os, (convLglines ast))

    fun translate01 (filename: string) =
        let
            val (ast, _) = Oldvb01.parse filename
        in
            convAll (TextIO.stdOut, ast)
        end

    fun translate02 (inFilename: string, outFilename: string) =
        let
            val (ast, _) = Oldvb01.parse inFilename
            val os = TextIO.openOut outFilename
        in
            convAll (os, ast);
            TextIO.closeOut os;
            print ("output \"" ^ outFilename ^ "\" success.\n")
        end


end (* structure Oldvb2Cs01 *)
