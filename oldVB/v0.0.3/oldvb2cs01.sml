structure Oldvb2Cs01 = 
struct

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
     
    fun convStmt (stmt: Absyn.statement) =
        case stmt of
            Absyn.LclVarDecl (v, t) =>
                (convVbType t) ^ " " ^ (convVar v) ^ ";"
            | Absyn.BlankLine => ""
            | Absyn.AssignStmt (v, e) => (convVar v) ^ " = " ^ (convExp e) ^ ";"

    fun convLgline (stmt: Absyn.statement, cmnt: Absyn.comment) =
        let 
            val sStmt = convStmt stmt
        in
            if "" = cmnt then sStmt ^ "\n"
            else sStmt ^ "//" ^ cmnt ^ "\n"
        end    

    fun printConv01 lglines =
        if null lglines then ()
        else 
            let 
                val lgl1::lgls = lglines
            in
                print (convLgline lgl1);
                printConv01 lgls
            end

    fun printConvAll (x, _) = printConv01 x


    fun translate01 (filename: string) =
        let
            val ast = Oldvb01.parse filename
        in
            printConvAll ast
        end

end (* structure Oldvb2Cs01 *)
