structure Tokens : MyLang_TOKENS =
struct
  (* A "scaffold" structure for debugging lexers. *)

    type linenum = int
    type token = string
    fun PLUS(i,j) = "PLUS   " ^ Int.toString(i)
    fun INT(c,i,j) = "INT("^Int.toString(c)^")   " ^ Int.toString(i)
    fun EOF(i,j) = "EOF   " ^ Int.toString(i)
end
