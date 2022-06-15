structure MwUtil =
struct

fun strJoin (sSep: string, lstStr: string list) : string =
    case lstStr of
        [] => ""
        | [s] => s
        | s::ss => s ^ sSep ^ strJoin (sSep, ss)

end        
