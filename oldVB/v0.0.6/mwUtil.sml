structure MwUtil =
struct

fun strJoin (sSep: string, lstStr: string list) : string =
    case lstStr of
        [] => ""
        | [s] => s
        | s::ss => s ^ sSep ^ strJoin (sSep, ss)

fun repeatStr (s: string) (c: int) = 
    if c = 0 then "" else s ^ (repeatStr s (c - 1))

end        


