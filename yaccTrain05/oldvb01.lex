structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)
val error = fn (e,l : int,_) =>
              TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^
                               ": " ^ e ^ "\n")

val f1 = fn (x, acc) => ord(x) - ord(#"0") + 10 * acc
val strToInt = fn (s : string) => foldl f1 0 (explode s)

%%
%header (functor Oldvb01LexFun(structure Tokens: Oldvb01_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%s STR COMMENT;
%full
%%
<INITIAL>\n       => (pos := (!pos) + 1; Tokens.LINE_TERM("line_term", (!pos)-1, (!pos)-1));
<INITIAL>{ws}+    => (lex());
<INITIAL>{digit}+ => (Tokens.INT ((strToInt yytext),!pos,!pos));
<INITIAL>"print"  => (Tokens.PRINT ("print", !pos, !pos));
<INITIAL>{alpha}+ => (Tokens.ID(yytext, yypos, yypos));
<INITIAL>"'"      => (YYBEGIN COMMENT; continue());
<INITIAL>"\""     => (YYBEGIN STR; continue());
<INITIAL>[^\ ]+   => (Tokens.ID(yytext, yypos, yypos));
<INITIAL>.        => (error ("ignoring bad character "^yytext,!pos,!pos); lex());
<COMMENT>\n       => (YYBEGIN INITIAL; Tokens.LINE_TERM("line_term", yypos, yypos));
<COMMENT>.        => (continue());
<STR>"\""      => (YYBEGIN INITIAL; continue());
<STR>{alpha}+  => (Tokens.STRING (yytext, yypos, yypos));
