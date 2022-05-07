signature MyLang_TOKENS =
sig
    type linenum (* = int *)
    type token
    val PLUS:  linenum * linenum -> token
    val INT: (int) *  linenum * linenum -> token
    val EOF:  linenum * linenum -> token
end
