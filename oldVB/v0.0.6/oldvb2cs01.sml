structure Oldvb2Cs01 = 
struct

    fun convProcName (procName: string) : string =     
        case procName of
            "Msgbox" => "Console.WriteLine"
            | _ => procName

    fun convSym (sym: Symbol.symbol) = Symbol.name sym

    fun convVbPrimType (t: Absyn.vbprimtype) =
        case t of
            Absyn.VbTyString => "string"
            | Absyn.VbTyInt => "int"

    fun convVbType (t: Absyn.vbtype) =
        case t of
            Absyn.VbTySimple pt => convVbPrimType pt
            | Absyn.VbTyArray pt => (convVbPrimType pt) ^ "[]" 

    fun convProcParamDec (param: Absyn.field) =
        let
            val sType = convVbType (#vbty param)
            val sParamName = convSym (#name param)
        in
            sType ^ " " ^ sParamName
        end
        
    fun convVar (v: Absyn.var) =
        case v of
            Absyn.SimpleVar (sym, _) => convSym sym
     
    fun convExp (e: Absyn.exp) =
        case e of
            Absyn.VarExp v => convVar v
            | Absyn.IntExp i => Int.toString i
            | Absyn.StringExp (s, _) => "\"" ^ s ^ "\""

    fun convProcParams (params: Absyn.exp list) =
        MwUtil.strJoin (", ", (map convExp params))
(*
        if null params then ""
        else
            let
                val p::ps = params
            in
                (convExp p) ^ "," ^ (convProcParams ps)
            end
*)
     
    fun convStmt (os: TextIO.outstream, idt:int, stmt: Absyn.statement) =
        case stmt of
            Absyn.LclVarDecl (v, t) => convLclVarDecl (os, idt, (v, t))
            | Absyn.ProcDec r => convProcDec (os, idt, r)
            | Absyn.BlankLine => (TextIO.output (os, "\n"); "")
            | Absyn.AssignStmt (v, e) => (TextIO.output (os, ((convVar v) ^ " = " ^ (convExp e) ^ ";\n")); "")
            | Absyn.CallProc (sym, params) =>
                let
                    val procName = convProcName (convSym sym)
                    val sProcStmt = procName ^ "(" ^ (convProcParams params) ^ ");\n"
                in
                     TextIO.output (os, sProcStmt);
                     ""
                end

    and convLclVarDecl (os: TextIO.outstream, idt:int, (v, t)) =
        let
            val sStmt = (convVbType t) ^ " " ^ (convVar v) ^ ";\n"
        in
            TextIO.output (os, sStmt);
            ""
        end
                
    and convProcDec (os: TextIO.outstream, idt:int, r) = 
        let
            val procName = convSym (#name r)
            val sParam = MwUtil.strJoin (", ", (map convProcParamDec (#params r)))
            val procHdr = "static void " ^ procName ^ "(" ^ sParam ^ ")\n{\n" 
        in
            TextIO.output (os, procHdr);
            convLglines (os, idt, (#body r))
            (* procHdr ^ body ^ "}" *)
        end

    and convLgline (os: TextIO.outstream, idt:int, (stmt: Absyn.statement, cmnt: Absyn.comment)) =
        (*
        let 
            val sStmt = convStmt (os, idt, stmt)
        in
            if "" = cmnt then sStmt ^ "\n"
            else sStmt ^ "//" ^ cmnt ^ "\n"
        end    
        *)
        (convStmt (os, idt, stmt);
        TextIO.output (os, cmnt))

(*
    and convLglines (lines: Absyn.lgline list) =
        if null lines then ""
        else
            let 
                val ln::lns = lines
            in
                (convLgline ln) ^ (convLglines lns)
            end
*)

    and convLglines (os: TextIO.outstream, idt:int, lines: Absyn.lgline list) =
        if null lines then ""
        else
            let 
                val ln::lns = lines
            in
                convLgline (os, idt, ln);
                convLglines (os, idt, lns);
                ""
            end


    fun convAll (os: TextIO.outstream, ast) =
        (TextIO.output (os, "namespace Oldvb2Cs01\n{\n");
        TextIO.output (os, "class Program\n{\n");
        (* TextIO.output (os, (convLglines ast)); *)
        convLglines (os, 0, ast);
        TextIO.output (os, "}\n");
        TextIO.output (os, "}\n"))

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
