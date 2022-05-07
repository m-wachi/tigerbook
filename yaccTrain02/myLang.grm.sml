functor myLangLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : myLang_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Sample interactive calculator for ML-Yacc *)

val flush_out = TextIO.flushOut
val std_out = TextIO.stdOut


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\004\000\000\000\
\\001\000\002\000\004\000\004\000\009\000\000\000\
\\001\000\003\000\010\000\004\000\010\000\000\000\
\\001\000\003\000\011\000\004\000\011\000\000\000\
\\001\000\003\000\005\000\004\000\008\000\000\000\
\\001\000\004\000\000\000\000\000\
\"
val actionRowNumbers =
"\001\000\004\000\002\000\000\000\
\\003\000\005\000"
val gotoT =
"\
\\001\000\001\000\002\000\005\000\000\000\
\\000\000\
\\000\000\
\\001\000\004\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 6
val numrules = 4
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 4) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
 $$ (T 0),nil
 $$ (T 4))::
(nil
,nil
 $$ (T 2))::
nil
val noShift = 
fn (T 3) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "PLUS"
  | (T 3) => "EOF"
  | (T 4) => "PRINT"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 0) => MlyValue.ID(fn () => ("bogus")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID EXP1, EXP1left, EXP1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (
EXP as EXP1) = EXP1 ()
 in (SOME EXP)
end; ()))
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => (
NONE))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( _, NUM1left, NUM1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => (NUM))
 in ( LrTable.NT 0, ( result, NUM1left, NUM1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.ntVOID EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.ntVOID EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (Tokens.PLUS(pos, pos+1))
end; ()))
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : myLang_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
end
end
