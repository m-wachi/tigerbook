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
        | BlankLine

type lgline = (statement * comment)

(*
datatype lgclline = LclVarDecl of var * vbtype
        | AssignStmt of var * exp
        | LinetermExp
        | CommentExp of string
*)


end
        
