type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

val f1 = fn (x, acc) => ord(x) - ord(#"0") + 10 * acc
val strToInt = fn (s : string) => foldl f1 0 (explode s)

%% 
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];

%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
{digit}+ => (Tokens.INT ((strToInt yytext),yypos,yypos+3));
"+"      => (Tokens.PLUS(yypos,yypos+1));
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

