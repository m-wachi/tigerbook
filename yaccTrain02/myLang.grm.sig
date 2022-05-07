signature myLang_TOKENS =
sig
type ('a,'b) token
type svalue
val PRINT:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val NUM:  'a * 'a -> (svalue,'a) token
val ID:  'a * 'a -> (svalue,'a) token
end
signature myLang_LRVALS=
sig
structure Tokens : myLang_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
