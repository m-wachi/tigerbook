structure Absyn = 
struct

type pos = int   and   symbol = Symbol.symbol

type comment = string

(*
type field = {name: symbol, escape: bool ref, 
		typ: symbol, pos: pos}
*)


datatype var = SimpleVar of symbol * pos

datatype vbprimtype = VbTyString | VbTyInt

datatype vbtype = VbTySimple of vbprimtype  | VbTyArray of vbprimtype

type field = {name: symbol, escape: bool ref, vbty: vbtype, pos: pos}

datatype oper = EqOp | NeqOp

datatype exp = VarExp of var
        | IntExp of int
        | StringExp of string * pos

(*
datatype defparam = DefParam of var * vbtype
*)
(*
datatype statement = LclVarDecl of var * vbtype
        | AssignStmt of var * exp
        | ProcDec of symbol * (defparam list) * (lgline list)
        | CallProc of symbol * (exp list)
        | BlankLine
withtype lgline = (statement * comment)
*)

datatype statement = LclVarDecl of var * vbtype
        | AssignStmt of var * exp
        | ProcDec of {
            name: symbol, params: field list,
            body: lgline list,
            pos: pos}
        | CallProc of symbol * (exp list)
        | BlankLine
withtype lgline = (statement * comment)


fun symToStr (sym: Symbol.symbol) =
    Symbol.name sym

fun vbprimtypeToStr (t: vbprimtype) : string =
    case t of 
        VbTyString => "String"
        | VbTyInt => "Integer"

fun vbtypeToStr (t: vbtype) : string =
    case t of 
        VbTySimple pt => vbprimtypeToStr pt
        | VbTyArray pt => "Array of " ^ (vbprimtypeToStr pt)

fun varToStr (v: var) =
    case v of
        SimpleVar (sym, _) => symToStr sym

fun expToStr (e: exp) =
    case e of 
        VarExp v => varToStr v
        | IntExp n => Int.toString n
        | StringExp (s, p) =>  "\"" ^ s ^ "\""

(*
fun defparamToStr (DefParam (v: var, t: vbtype)) =
    (varToStr v) ^ ":" ^ (vbtypeToStr t)
*)
fun fieldToStr (fld: field) =
    (symToStr (#name fld)) ^ ":" ^ (vbtypeToStr (#vbty fld))
   
(*        
fun stmtToStr (stmt: statement) =
    case stmt of
        LclVarDecl (v, t) =>
            "LclVarDecl var=" ^ (varToStr v)
        | ProcDec (sym, params, lines) =>
            let
                val procName = symToStr sym
                val sParam = MwUtil.strJoin (", ", (map defparamToStr params))
                val procHdr = "ProcDec " ^ procName ^ "(" ^ sParam ^ ")\n" 
                val body = lglinesToStr lines
            in
                procHdr ^ body ^ "End ProcDec " ^ procName
            end
        | AssignStmt (v, e) =>
            "AssignStmt right=" ^ (varToStr v) ^ ", left=" ^ (expToStr e)
        | CallProc (sym, params) =>
            "CallProc " ^ (symToStr sym) ^ "(" ^ (procParamsToStr params) ^ ")"
        | BlankLine => "BlankLine"
*)

fun stmtToStr (stmt: statement) =
    case stmt of
        LclVarDecl (v, t) =>
            "LclVarDecl var=" ^ (varToStr v)
        | ProcDec r => procDecToStr r
        | AssignStmt (v, e) =>
            "AssignStmt right=" ^ (varToStr v) ^ ", left=" ^ (expToStr e)
        | CallProc (sym, params) =>
            "CallProc " ^ (symToStr sym) ^ "(" ^ (procParamsToStr params) ^ ")"
        | BlankLine => "BlankLine"

and procDecToStr r =
    let
        val procName = symToStr (#name r)
        val sParam = MwUtil.strJoin (", ", (map fieldToStr (#params r)))
        val procHdr = "ProcDec " ^ procName ^ "(" ^ sParam ^ ")\n" 
        val body = lglinesToStr (#body r)
    in
        procHdr ^ body ^ "End ProcDec " ^ procName
    end

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
        
