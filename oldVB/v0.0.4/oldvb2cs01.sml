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
                
(*
    fun printConv01 lglines =
        if null lglines then ()
        else 
            let 
                val lgl1::lgls = lglines
            in
                print (convLgline lgl1);
                printConv01 lgls
            end
*)
    fun printConvAll (x, _) = print (convLglines x)


    fun translate01 (filename: string) =
        let
            val ast = Oldvb01.parse filename
        in
            printConvAll ast
        end

end (* structure Oldvb2Cs01 *)
