val (b, c) = case prog
    of CompoundStm (x, y) => (x, y)

fun procCompoundStm x =
    case x
        of CompoundStm(s1, s2) => (s1, s2)

fun procStm x =
    case x
        of CompoundStm(s1, s2) => ()  (* s1, s2 *)
        | AssignStm(a1, a2) => ()
	| PrintStm elist => ()
