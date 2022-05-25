structure Absyn = 
struct

type pos = int   and   symbol = Symbol.symbol

datatype var = SimpleVar of symbol * pos

datatype exp = VarExp of var
        | IntExp of int
        | StringExp of string * pos
     
end
        
