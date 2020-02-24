functor FunLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Fun_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn
structure S = Symbol
structure E = ErrorMsg


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\122\000\005\000\122\000\006\000\122\000\007\000\122\000\
\\008\000\122\000\009\000\122\000\010\000\122\000\011\000\122\000\
\\012\000\122\000\013\000\122\000\014\000\122\000\015\000\122\000\
\\016\000\122\000\018\000\122\000\019\000\122\000\022\000\122\000\
\\023\000\122\000\025\000\122\000\027\000\122\000\029\000\122\000\
\\032\000\122\000\000\000\
\\001\000\001\000\124\000\005\000\124\000\006\000\124\000\007\000\118\000\
\\008\000\124\000\009\000\118\000\010\000\124\000\011\000\124\000\
\\012\000\124\000\013\000\124\000\014\000\124\000\015\000\118\000\
\\016\000\124\000\018\000\124\000\019\000\124\000\022\000\124\000\
\\023\000\124\000\025\000\124\000\027\000\124\000\028\000\118\000\
\\029\000\124\000\031\000\118\000\032\000\124\000\000\000\
\\001\000\001\000\142\000\005\000\142\000\006\000\142\000\007\000\114\000\
\\008\000\142\000\009\000\114\000\010\000\142\000\011\000\142\000\
\\012\000\142\000\013\000\142\000\014\000\142\000\015\000\114\000\
\\016\000\142\000\018\000\142\000\019\000\142\000\022\000\142\000\
\\023\000\142\000\025\000\142\000\027\000\142\000\028\000\114\000\
\\029\000\142\000\031\000\114\000\032\000\142\000\000\000\
\\001\000\001\000\008\000\000\000\
\\001\000\001\000\010\000\000\000\
\\001\000\001\000\016\000\006\000\015\000\008\000\014\000\014\000\013\000\000\000\
\\001\000\001\000\016\000\006\000\015\000\008\000\014\000\014\000\013\000\
\\015\000\022\000\000\000\
\\001\000\001\000\016\000\006\000\056\000\007\000\087\000\008\000\014\000\
\\009\000\055\000\010\000\054\000\011\000\053\000\012\000\052\000\
\\013\000\051\000\014\000\050\000\016\000\049\000\018\000\048\000\
\\019\000\047\000\029\000\046\000\000\000\
\\001\000\001\000\016\000\006\000\056\000\007\000\089\000\008\000\014\000\
\\009\000\055\000\010\000\054\000\011\000\053\000\012\000\052\000\
\\013\000\051\000\014\000\050\000\016\000\049\000\018\000\048\000\
\\019\000\047\000\029\000\046\000\000\000\
\\001\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\010\000\054\000\011\000\053\000\012\000\052\000\013\000\051\000\
\\014\000\050\000\015\000\086\000\016\000\049\000\018\000\048\000\
\\019\000\047\000\029\000\046\000\000\000\
\\001\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\010\000\054\000\011\000\053\000\012\000\052\000\013\000\051\000\
\\014\000\050\000\016\000\049\000\018\000\048\000\019\000\047\000\
\\022\000\085\000\029\000\046\000\000\000\
\\001\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\010\000\054\000\011\000\053\000\012\000\052\000\013\000\051\000\
\\014\000\050\000\016\000\049\000\018\000\048\000\019\000\047\000\
\\022\000\101\000\029\000\046\000\000\000\
\\001\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\010\000\054\000\011\000\053\000\012\000\052\000\013\000\051\000\
\\014\000\050\000\016\000\049\000\018\000\048\000\019\000\047\000\
\\025\000\084\000\029\000\046\000\000\000\
\\001\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\010\000\054\000\011\000\053\000\012\000\052\000\013\000\051\000\
\\014\000\050\000\016\000\049\000\018\000\048\000\019\000\047\000\
\\027\000\096\000\029\000\046\000\000\000\
\\001\000\001\000\044\000\002\000\043\000\003\000\042\000\006\000\041\000\
\\012\000\040\000\014\000\039\000\015\000\064\000\020\000\038\000\
\\021\000\037\000\024\000\036\000\026\000\035\000\030\000\034\000\
\\031\000\033\000\000\000\
\\001\000\001\000\044\000\002\000\043\000\003\000\042\000\006\000\041\000\
\\012\000\040\000\014\000\039\000\020\000\038\000\021\000\037\000\
\\024\000\036\000\026\000\035\000\030\000\034\000\031\000\033\000\000\000\
\\001\000\001\000\044\000\002\000\043\000\003\000\042\000\006\000\041\000\
\\012\000\040\000\014\000\039\000\020\000\038\000\021\000\095\000\
\\024\000\036\000\026\000\035\000\030\000\034\000\031\000\033\000\000\000\
\\001\000\001\000\059\000\000\000\
\\001\000\001\000\066\000\002\000\065\000\000\000\
\\001\000\001\000\076\000\002\000\043\000\003\000\042\000\006\000\075\000\
\\008\000\014\000\012\000\040\000\014\000\074\000\015\000\022\000\
\\020\000\038\000\021\000\037\000\024\000\036\000\026\000\035\000\
\\030\000\034\000\031\000\033\000\000\000\
\\001\000\001\000\076\000\002\000\043\000\003\000\042\000\006\000\075\000\
\\008\000\014\000\012\000\040\000\014\000\074\000\015\000\088\000\
\\020\000\038\000\021\000\037\000\024\000\036\000\026\000\035\000\
\\030\000\034\000\031\000\033\000\000\000\
\\001\000\001\000\076\000\002\000\043\000\003\000\042\000\006\000\075\000\
\\008\000\014\000\012\000\040\000\014\000\074\000\020\000\038\000\
\\021\000\037\000\024\000\036\000\026\000\035\000\030\000\034\000\
\\031\000\033\000\000\000\
\\001\000\005\000\005\000\000\000\
\\001\000\005\000\005\000\032\000\007\000\000\000\
\\001\000\006\000\009\000\000\000\
\\001\000\007\000\020\000\009\000\019\000\028\000\018\000\031\000\017\000\000\000\
\\001\000\007\000\029\000\009\000\019\000\028\000\018\000\031\000\017\000\000\000\
\\001\000\008\000\011\000\000\000\
\\001\000\008\000\027\000\000\000\
\\001\000\009\000\019\000\015\000\028\000\028\000\018\000\031\000\017\000\000\000\
\\001\000\009\000\019\000\016\000\031\000\028\000\018\000\031\000\017\000\000\000\
\\001\000\016\000\083\000\000\000\
\\001\000\032\000\000\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\010\000\054\000\011\000\053\000\012\000\052\000\013\000\051\000\
\\014\000\050\000\016\000\049\000\018\000\048\000\019\000\047\000\
\\029\000\046\000\000\000\
\\111\000\009\000\019\000\028\000\018\000\031\000\017\000\000\000\
\\112\000\000\000\
\\113\000\009\000\019\000\028\000\018\000\031\000\017\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\009\000\019\000\028\000\018\000\031\000\017\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\010\000\054\000\011\000\053\000\012\000\052\000\013\000\051\000\
\\014\000\050\000\016\000\049\000\018\000\048\000\019\000\047\000\
\\023\000\097\000\029\000\046\000\000\000\
\\119\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\010\000\054\000\011\000\053\000\012\000\052\000\013\000\051\000\
\\014\000\050\000\016\000\049\000\018\000\048\000\019\000\047\000\
\\023\000\104\000\029\000\046\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\
\\016\000\049\000\018\000\048\000\019\000\047\000\029\000\046\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\001\000\016\000\006\000\056\000\009\000\055\000\011\000\053\000\
\\012\000\052\000\013\000\051\000\014\000\050\000\016\000\049\000\000\000\
\\130\000\001\000\016\000\006\000\056\000\009\000\055\000\000\000\
\\131\000\001\000\016\000\006\000\056\000\009\000\055\000\000\000\
\\132\000\001\000\016\000\006\000\056\000\009\000\055\000\013\000\051\000\000\000\
\\133\000\001\000\016\000\006\000\056\000\009\000\055\000\013\000\051\000\000\000\
\\134\000\001\000\016\000\006\000\056\000\009\000\055\000\000\000\
\\135\000\001\000\016\000\006\000\056\000\009\000\055\000\011\000\053\000\
\\012\000\052\000\013\000\051\000\014\000\050\000\016\000\049\000\000\000\
\\136\000\001\000\016\000\006\000\056\000\009\000\055\000\011\000\053\000\
\\012\000\052\000\013\000\051\000\014\000\050\000\016\000\049\000\000\000\
\\137\000\001\000\016\000\006\000\056\000\009\000\055\000\011\000\053\000\
\\012\000\052\000\013\000\051\000\000\000\
\\138\000\001\000\016\000\006\000\056\000\009\000\055\000\011\000\053\000\
\\012\000\052\000\013\000\051\000\000\000\
\\139\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\
\\016\000\049\000\018\000\048\000\019\000\047\000\000\000\
\\140\000\000\000\
\\141\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\010\000\054\000\011\000\053\000\012\000\052\000\013\000\051\000\
\\014\000\050\000\016\000\049\000\018\000\048\000\019\000\047\000\
\\029\000\046\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\009\000\019\000\028\000\018\000\031\000\017\000\000\000\
\\145\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\
\\016\000\049\000\018\000\048\000\019\000\047\000\029\000\046\000\000\000\
\\146\000\000\000\
\\147\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\
\\016\000\049\000\018\000\048\000\019\000\047\000\029\000\046\000\000\000\
\\148\000\001\000\016\000\006\000\056\000\008\000\014\000\009\000\055\000\
\\010\000\054\000\011\000\053\000\012\000\052\000\013\000\051\000\
\\014\000\050\000\016\000\049\000\018\000\048\000\019\000\047\000\
\\029\000\046\000\000\000\
\\149\000\001\000\016\000\006\000\056\000\009\000\055\000\000\000\
\"
val actionRowNumbers =
"\022\000\035\000\023\000\003\000\
\\034\000\033\000\024\000\004\000\
\\027\000\005\000\025\000\006\000\
\\005\000\005\000\044\000\043\000\
\\005\000\005\000\028\000\029\000\
\\040\000\037\000\026\000\042\000\
\\039\000\005\000\041\000\038\000\
\\030\000\015\000\036\000\015\000\
\\015\000\017\000\015\000\015\000\
\\015\000\014\000\018\000\015\000\
\\015\000\051\000\050\000\070\000\
\\015\000\015\000\015\000\015\000\
\\019\000\015\000\015\000\015\000\
\\015\000\015\000\021\000\075\000\
\\056\000\031\000\012\000\010\000\
\\055\000\009\000\068\000\053\000\
\\054\000\007\000\057\000\065\000\
\\062\000\061\000\063\000\064\000\
\\020\000\021\000\001\000\060\000\
\\059\000\058\000\052\000\067\000\
\\008\000\015\000\015\000\016\000\
\\066\000\049\000\002\000\069\000\
\\013\000\073\000\047\000\045\000\
\\072\000\015\000\015\000\015\000\
\\011\000\074\000\071\000\016\000\
\\046\000\000\000\016\000\048\000\
\\032\000"
val gotoT =
"\
\\001\000\104\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\003\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\010\000\000\000\
\\000\000\
\\007\000\019\000\000\000\
\\007\000\021\000\000\000\
\\007\000\022\000\000\000\
\\000\000\
\\000\000\
\\007\000\023\000\000\000\
\\007\000\024\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\030\000\000\000\
\\007\000\043\000\000\000\
\\005\000\055\000\000\000\
\\005\000\056\000\000\000\
\\000\000\
\\005\000\058\000\000\000\
\\005\000\059\000\000\000\
\\005\000\060\000\000\000\
\\005\000\061\000\000\000\
\\000\000\
\\005\000\065\000\000\000\
\\005\000\066\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\067\000\000\000\
\\005\000\068\000\000\000\
\\005\000\069\000\000\000\
\\005\000\070\000\000\000\
\\005\000\071\000\007\000\019\000\000\000\
\\005\000\075\000\000\000\
\\005\000\076\000\000\000\
\\005\000\077\000\000\000\
\\005\000\078\000\000\000\
\\005\000\079\000\000\000\
\\005\000\080\000\007\000\022\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\005\000\061\000\007\000\019\000\000\000\
\\005\000\065\000\007\000\022\000\000\000\
\\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\005\000\088\000\000\000\
\\005\000\089\000\000\000\
\\004\000\092\000\005\000\091\000\006\000\090\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\000\000\
\\007\000\043\000\000\000\
\\000\000\
\\005\000\096\000\000\000\
\\005\000\097\000\000\000\
\\005\000\098\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\007\000\043\000\000\000\
\\004\000\101\000\005\000\100\000\006\000\090\000\000\000\
\\007\000\043\000\000\000\
\\000\000\
\\005\000\098\000\006\000\103\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 105
val numrules = 43
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
type pos = E.pos
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | POS of unit ->  (int*int) | PROJ of unit ->  (int)
 | INT of unit ->  (int) | ID of unit ->  (string)
 | TYPE of unit ->  (A.tp) | UNBAL_EXP of unit ->  (A.exp)
 | EXP of unit ->  (A.exp) | STM of unit ->  (A.exp)
 | FUNDEC of unit ->  (A.fundec)
 | FUNDECLIST of unit ->  (A.fundec list)
 | PROG of unit ->  (A.fundec list)
end
type svalue = MlyValue.svalue
type result = A.fundec list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 31) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "INT"
  | (T 2) => "PROJ"
  | (T 3) => "POS"
  | (T 4) => "FUN"
  | (T 5) => "LPAREN"
  | (T 6) => "RPAREN"
  | (T 7) => "COLON"
  | (T 8) => "COMMA"
  | (T 9) => "SEMICOLON"
  | (T 10) => "PLUS"
  | (T 11) => "MINUS"
  | (T 12) => "TIMES"
  | (T 13) => "LT"
  | (T 14) => "GT"
  | (T 15) => "EQ"
  | (T 16) => "UMINUS"
  | (T 17) => "AND"
  | (T 18) => "OR"
  | (T 19) => "NOT"
  | (T 20) => "IF"
  | (T 21) => "THEN"
  | (T 22) => "ELSE"
  | (T 23) => "WHILE"
  | (T 24) => "DO"
  | (T 25) => "LET"
  | (T 26) => "IN"
  | (T 27) => "ARROW"
  | (T 28) => "ASSIGN"
  | (T 29) => "BANG"
  | (T 30) => "REF"
  | (T 31) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25)
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18)
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, EOF1right)) :: ( _, ( MlyValue.FUNDECLIST 
FUNDECLIST1, FUNDECLIST1left, _)) :: rest671)) => let val  result = 
MlyValue.PROG (fn _ => let val  (FUNDECLIST as FUNDECLIST1) = 
FUNDECLIST1 ()
 in (FUNDECLIST)
end)
 in ( LrTable.NT 0, ( result, FUNDECLIST1left, EOF1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.FUNDEC FUNDEC1, _, FUNDEC1right)) :: ( _, ( 
MlyValue.FUNDECLIST FUNDECLIST1, FUNDECLIST1left, _)) :: rest671)) =>
 let val  result = MlyValue.FUNDECLIST (fn _ => let val  (FUNDECLIST
 as FUNDECLIST1) = FUNDECLIST1 ()
 val  (FUNDEC as FUNDEC1) = FUNDEC1 ()
 in (FUNDECLIST @ [FUNDEC])
end)
 in ( LrTable.NT 1, ( result, FUNDECLIST1left, FUNDEC1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.FUNDEC FUNDEC1, FUNDEC1left, FUNDEC1right))
 :: rest671)) => let val  result = MlyValue.FUNDECLIST (fn _ => let
 val  (FUNDEC as FUNDEC1) = FUNDEC1 ()
 in ([FUNDEC])
end)
 in ( LrTable.NT 1, ( result, FUNDEC1left, FUNDEC1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.EXP EXP1, _, (EXPright as EXP1right))) :: _
 :: ( _, ( MlyValue.TYPE TYPE2, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.TYPE TYPE1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FUNleft as FUN1left
), _)) :: rest671)) => let val  result = MlyValue.FUNDEC (fn _ => let
 val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  TYPE1 = TYPE1 ()
 val  TYPE2 = TYPE2 ()
 val  (EXP as EXP1) = EXP1 ()
 in (
((FUNleft, EXPright), (S.symbol ID1, S.symbol ID2, TYPE1, TYPE2, EXP))
)
end)
 in ( LrTable.NT 2, ( result, FUN1left, EXP1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.TYPE TYPE1, _, TYPE1right)) :: ( _, ( _, 
COLON1left, _)) :: rest671)) => let val  result = MlyValue.TYPE (fn _
 => let val  (TYPE as TYPE1) = TYPE1 ()
 in (TYPE)
end)
 in ( LrTable.NT 6, ( result, COLON1left, TYPE1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.TYPE TYPE1, _
, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.TYPE (fn _ => let val  (TYPE as TYPE1) = TYPE1 ()
 in (TYPE)
end)
 in ( LrTable.NT 6, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.TYPE TYPE2, _, TYPE2right)) :: _ :: ( _, ( 
MlyValue.TYPE TYPE1, TYPE1left, _)) :: rest671)) => let val  result = 
MlyValue.TYPE (fn _ => let val  TYPE1 = TYPE1 ()
 val  TYPE2 = TYPE2 ()
 in (A.Tupletp [TYPE1, TYPE2])
end)
 in ( LrTable.NT 6, ( result, TYPE1left, TYPE2right), rest671)
end
|  ( 7, ( ( _, ( _, _, GT1right)) :: ( _, ( _, LT1left, _)) :: rest671
)) => let val  result = MlyValue.TYPE (fn _ => (A.Tupletp nil))
 in ( LrTable.NT 6, ( result, LT1left, GT1right), rest671)
end
|  ( 8, ( ( _, ( _, _, GT1right)) :: ( _, ( MlyValue.TYPE TYPE1, _, _)
) :: ( _, ( _, LT1left, _)) :: rest671)) => let val  result = 
MlyValue.TYPE (fn _ => let val  (TYPE as TYPE1) = TYPE1 ()
 in (A.Tupletp [TYPE])
end)
 in ( LrTable.NT 6, ( result, LT1left, GT1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.TYPE TYPE2, _, TYPE2right)) :: _ :: ( _, ( 
MlyValue.TYPE TYPE1, TYPE1left, _)) :: rest671)) => let val  result = 
MlyValue.TYPE (fn _ => let val  TYPE1 = TYPE1 ()
 val  TYPE2 = TYPE2 ()
 in (A.Arrowtp(TYPE1, TYPE2))
end)
 in ( LrTable.NT 6, ( result, TYPE1left, TYPE2right), rest671)
end
|  ( 10, ( ( _, ( _, _, REF1right)) :: ( _, ( MlyValue.TYPE TYPE1, 
TYPE1left, _)) :: rest671)) => let val  result = MlyValue.TYPE (fn _
 => let val  (TYPE as TYPE1) = TYPE1 ()
 in (A.Reftp (TYPE))
end)
 in ( LrTable.NT 6, ( result, TYPE1left, REF1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.TYPE (fn _ => let val  ID1 = ID1 ()
 in (A.Inttp)
end)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671)
) => let val  result = MlyValue.STM (fn _ => let val  (EXP as EXP1) = 
EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.UNBAL_EXP UNBAL_EXP1, UNBAL_EXP1left, 
UNBAL_EXP1right)) :: rest671)) => let val  result = MlyValue.STM (fn _
 => let val  (UNBAL_EXP as UNBAL_EXP1) = UNBAL_EXP1 ()
 in (UNBAL_EXP)
end)
 in ( LrTable.NT 3, ( result, UNBAL_EXP1left, UNBAL_EXP1right), 
rest671)
end
|  ( 14, ( ( _, ( MlyValue.UNBAL_EXP UNBAL_EXP1, _, (UNBAL_EXPright
 as UNBAL_EXP1right))) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _
 :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, (IFleft as IF1left),
 _)) :: rest671)) => let val  result = MlyValue.UNBAL_EXP (fn _ => let
 val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 val  (UNBAL_EXP as UNBAL_EXP1) = UNBAL_EXP1 ()
 in (A.Pos((IFleft, UNBAL_EXPright), A.If(EXP1, EXP2, UNBAL_EXP)))
end
)
 in ( LrTable.NT 5, ( result, IF1left, UNBAL_EXP1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.STM STM1, _, (STMright as STM1right))) :: _
 :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, (IFleft as IF1left),
 _)) :: rest671)) => let val  result = MlyValue.UNBAL_EXP (fn _ => let
 val  (EXP as EXP1) = EXP1 ()
 val  (STM as STM1) = STM1 ()
 in (A.Pos((IFleft, STMright), A.If(EXP, STM, A.Tuple([]))))
end)
 in ( LrTable.NT 5, ( result, IF1left, STM1right), rest671)
end
|  ( 16, ( ( _, ( _, _, (RPARENright as RPAREN1right))) :: ( _, ( 
MlyValue.EXP EXP1, _, _)) :: ( _, ( _, (LPARENleft as LPAREN1left), _)
) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (
EXP as EXP1) = EXP1 ()
 in (A.Pos((LPARENleft, RPARENright), EXP))
end)
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), (IDright as 
ID1right))) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  (ID as ID1) = ID1 ()
 in (A.Pos((IDleft, IDright), A.Id(S.symbol (ID))))
end)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.INT INT1, (INTleft as INT1left), (INTright
 as INT1right))) :: rest671)) => let val  result = MlyValue.EXP (fn _
 => let val  (INT as INT1) = INT1 ()
 in (A.Pos((INTleft, INTright), A.Int(INT)))
end)
 in ( LrTable.NT 4, ( result, INT1left, INT1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
A.Pos((EXP1left, EXP2right), EXP1); A.Pos((EXP1left, EXP2right), EXP2)
)
end)
 in ( LrTable.NT 4, ( result, EXP1left, EXP2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.INT INT1, _, (INTright as INT1right))) :: (
 _, ( _, (MINUSleft as MINUS1left), _)) :: rest671)) => let val  
result = MlyValue.EXP (fn _ => let val  (INT as INT1) = INT1 ()
 in (A.Pos((MINUSleft, INTright), A.Int(~INT)))
end)
 in ( LrTable.NT 4, ( result, MINUS1left, INT1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ID ID1, _, (IDright as ID1right))) :: ( _, 
( _, (MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.Pos((MINUSleft, IDright), A.Id(S.symbol (ID))))
end)
 in ( LrTable.NT 4, ( result, MINUS1left, ID1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP1, _, (EXPright as EXP1right))) :: (
 _, ( _, (NOTleft as NOT1left), _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (
if EXP = A.Int(0) then A.Pos((NOTleft, EXPright), A.Int(1)) else A.Pos((NOTleft, EXPright), A.Int(0))
)
end)
 in ( LrTable.NT 4, ( result, NOT1left, EXP1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXP EXP1, _, (EXPright as EXP1right))) :: (
 _, ( _, (BANGleft as BANG1left), _)) :: rest671)) => let val  result
 = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (A.Pos((BANGleft, EXPright), A.Op(A.Get, [EXP])))
end)
 in ( LrTable.NT 4, ( result, BANG1left, EXP1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXP EXP1, _, (EXPright as EXP1right))) :: (
 _, ( MlyValue.PROJ PROJ1, (PROJleft as PROJ1left), _)) :: rest671))
 => let val  result = MlyValue.EXP (fn _ => let val  (PROJ as PROJ1) =
 PROJ1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (A.Pos((PROJleft, EXPright), A.Proj(PROJ, EXP)))
end)
 in ( LrTable.NT 4, ( result, PROJ1left, EXP1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (A.Pos((EXP1left, EXP2right), A.Op(A.Add, [EXP1, EXP2])))
end)
 in ( LrTable.NT 4, ( result, EXP1left, EXP2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (A.Pos((EXP1left, EXP2right), A.Op(A.Sub, [EXP1, EXP2])))
end)
 in ( LrTable.NT 4, ( result, EXP1left, EXP2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (A.Pos((EXP1left, EXP2right), A.Op(A.Mul, [EXP1, EXP2])))
end)
 in ( LrTable.NT 4, ( result, EXP1left, EXP2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
let val e1 = EXP1; val e2 = EXP2; in if e1 = A.Int(0) then A.Pos((EXP1left, EXP2right), A.Int(0)) else
                                                        if e2 = A.Int(0) then A.Pos((EXP1left, EXP2right), A.Int(0)) else 
                                                        A.Pos((EXP1left, EXP2right), A.Int(0)) end
)
end)
 in ( LrTable.NT 4, ( result, EXP1left, EXP2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
let val e1 = EXP1; val e2 = EXP2; in if e1 = A.Int(0) then if e2 = A.Int(0) then A.Pos((EXP1left, EXP2right), A.Int(0)) else
                                                        A.Pos((EXP1left, EXP2right), A.Int(1)) else A.Pos((EXP1left, EXP2right), A.Int(1)) end
)
end)
 in ( LrTable.NT 4, ( result, EXP1left, EXP2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (A.Pos((EXP1left, EXP2right), A.Op(A.Eq, [EXP1, EXP2])))
end)
 in ( LrTable.NT 4, ( result, EXP1left, EXP2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (A.Pos((EXP1left, EXP2right), A.Op(A.LT, [EXP1, EXP2])))
end)
 in ( LrTable.NT 4, ( result, EXP1left, EXP2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (A.Pos((EXP1left, EXP2right), A.Op(A.Set, [EXP1, EXP2])))
end)
 in ( LrTable.NT 4, ( result, EXP1left, EXP2right), rest671)
end
|  ( 33, ( ( _, ( _, _, (GTright as GT1right))) :: ( _, ( MlyValue.EXP
 EXP1, _, _)) :: ( _, ( _, (LTleft as LT1left), _)) :: rest671)) =>
 let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1
 ()
 in (A.Pos((LTleft, GTright), A.Tuple(EXP::nil)))
end)
 in ( LrTable.NT 4, ( result, LT1left, GT1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (A.Pos((EXP1left, EXP2right), A.Tuple([EXP1, EXP2])))
end)
 in ( LrTable.NT 4, ( result, EXP1left, EXP2right), rest671)
end
|  ( 35, ( ( _, ( _, _, (GTright as GT1right))) :: ( _, ( _, (LTleft
 as LT1left), _)) :: rest671)) => let val  result = MlyValue.EXP (fn _
 => (A.Pos((LTleft, GTright), A.Tuple([]))))
 in ( LrTable.NT 4, ( result, LT1left, GT1right), rest671)
end
|  ( 36, ( ( _, ( _, _, (RPARENright as RPAREN1right))) :: ( _, ( 
MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left,
 _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  
EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (A.Pos((EXP1left, RPARENright), A.Call(EXP1, EXP2)))
end)
 in ( LrTable.NT 4, ( result, EXP1left, RPAREN1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.TYPE TYPE1, _, (TYPEright as TYPE1right)))
 :: ( _, ( MlyValue.EXP EXP1, (EXPleft as EXP1left), _)) :: rest671))
 => let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = 
EXP1 ()
 val  (TYPE as TYPE1) = TYPE1 ()
 in (A.Pos((EXPleft, TYPEright), A.Constrain(EXP, TYPE)))
end)
 in ( LrTable.NT 4, ( result, EXP1left, TYPE1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( 
MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: 
( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 val  EXP3 = EXP3 ()
 in (A.Pos((IFleft, EXP3right), A.If(EXP1, EXP2, EXP3)))
end)
 in ( LrTable.NT 4, ( result, IF1left, EXP3right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.STM STM1, _, (STMright as STM1right))) :: _
 :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, (IFleft as IF1left),
 _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val 
 (EXP as EXP1) = EXP1 ()
 val  (STM as STM1) = STM1 ()
 in (A.Pos((IFleft, STMright), A.If(EXP, STM, A.Tuple([]))))
end)
 in ( LrTable.NT 4, ( result, IF1left, STM1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _))
 :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1
 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (A.Pos((WHILEleft, EXP2right), A.While(EXP1, EXP2)))
end)
 in ( LrTable.NT 4, ( result, WHILE1left, EXP2right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: (
 _, ( _, (LETleft as LET1left), _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (A.Pos((LETleft, EXP2right), A.Let(S.symbol(ID), EXP1, EXP2)))
end
)
 in ( LrTable.NT 4, ( result, LET1left, EXP2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.EXP EXP1, _, (EXPright as EXP1right))) :: (
 _, ( _, (REFleft as REF1left), _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (A.Pos((REFleft, EXPright), A.Op(A.Ref, [EXP])))
end)
 in ( LrTable.NT 4, ( result, REF1left, EXP1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROG x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Fun_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun PROJ (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.PROJ (fn () => i),p1,p2))
fun POS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.POS (fn () => i),p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun BANG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun REF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
end
end
