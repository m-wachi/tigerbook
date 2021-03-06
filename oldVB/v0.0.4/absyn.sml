structure Absyn = 
struct

type pos = int   and   symbol = Symbol.symbol

type comment = string

datatype var = SimpleVar of symbol * pos

datatype vbtype = VbTyString | VbTyInt

datatype oper = EqOp | NeqOp

datatype exp = VarExp of var
        | IntExp of int
        | StringExp of string * pos

datatype statement = LclVarDecl of var * vbtype
        | AssignStmt of var * exp
        | DefProc of symbol * (lgline list)
        | CallProc of symbol * (exp list)
        | BlankLine
withtype lgline = (statement * comment)

(*
datatype lgclline = LclVarDecl of var * vbtype
        | AssignStmt of var * exp
        | LinetermExp
        | CommentExp of string
*)



fun symToStr (sym: Symbol.symbol) =
    Symbol.name sym

fun varToStr (v: var) =
    case v of
        SimpleVar (sym, _) => symToStr sym

fun expToStr (e: exp) =
    case e of 
        VarExp v => varToStr v
        | IntExp n => Int.toString n
        | StringExp (s, p) =>  "\"" ^ s ^ "\""
        
fun stmtToStr (stmt: statement) =
    case stmt of
        LclVarDecl (v, t) =>
            "LclVarDecl var=" ^ (varToStr v)
        | DefProc (sym, lines) =>
            let
	        val procName = symToStr sym
                val procHdr = "DefProc " ^ procName ^ "()\n" 
                val body = lglinesToStr lines
            in
                procHdr ^ body ^ "End DefProc " ^ procName
            end
        | AssignStmt (v, e) =>
            "AssignStmt right=" ^ (varToStr v) ^ ", left=" ^ (expToStr e)
        | CallProc (sym, params) =>
            "CallProc " ^ (symToStr sym) ^ "(" ^ (procParamsToStr params) ^ ")"
        | BlankLine => "BlankLine"

and procParamsToStr (params: exp list) =
    if null params then ""
    else
        let
            val p::ps = params
        in
            (expToStr p) ^ "," ^ (procParamsToStr ps)
        end
         

and lglineToStr (stmt: statement, cmnt: comment) =
    "statement: " ^ (stmtToStr stmt) 
        ^ "\ncomment: " ^ cmnt ^ "\n"

and lglinesToStr (lglines: lgline list) =
    if null lglines then ""
    else
        let 
            val lgl1::lgls = lglines
        in
            (lglineToStr lgl1) ^ (lglinesToStr lgls)
        end

end
        
