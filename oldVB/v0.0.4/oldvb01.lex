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
<INITIAL>\n       => (pos := (!pos) + 1; Tokens.LINE_TERM(yypos, yypos));
<INITIAL>"'"      => (YYBEGIN COMMENT; continue());
<INITIAL>"\"\""   => (Tokens.STRING ("", yypos, yypos));
<INITIAL>"\""     => (YYBEGIN STR; continue());
<INITIAL>"="      => (Tokens.EQUAL(yypos, yypos));
<INITIAL>Integer  => (Tokens.TY_INT(yypos, yypos));
<INITIAL>String   => (Tokens.TY_STRING(yypos, yypos));
<INITIAL>Dim      => (Tokens.DIM(yypos, yypos));
<INITIAL>As       => (Tokens.AS(yypos, yypos));
<INITIAL>{ws}+    => (lex());
<INITIAL>{digit}+ => (Tokens.INT ((strToInt yytext),yypos,yypos));
<INITIAL>"print"  => (Tokens.PRINT ("print", yypos, yypos));
<INITIAL>({alpha}|".")+ => (Tokens.ID(yytext, yypos, yypos));
<INITIAL>.        => (error ("ignoring bad character "^yytext,yypos,yypos); lex());
<COMMENT>\n       => (YYBEGIN INITIAL; Tokens.LINE_TERM(yypos, yypos));
<COMMENT>.*        => (Tokens.COMMENT (yytext, yypos, yypos));
<STR>([^\n\"]|"\"\"")+  => (Tokens.STRING (yytext, yypos, yypos));
<STR>"\""      => (YYBEGIN INITIAL; continue());
<STR>\n        => (error ("error newline in string",yypos,yypos); lex());
