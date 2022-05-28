structure Absyn = 
struct

type pos = int   and   symbol = Symbol.symbol

datatype var = SimpleVar of symbol * pos

datatype oper = EqOp | NeqOp

datatype exp = VarExp of var
        | IntExp of int
        | StringExp of string * pos
        | LinetermExp
        | CommentExp of string

datatype stmt = AssignStmt of var * exp

end
        
