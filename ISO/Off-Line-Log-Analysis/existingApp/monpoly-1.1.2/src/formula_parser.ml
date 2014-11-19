type token =
  | LPA
  | RPA
  | LSB
  | RSB
  | COM
  | DOT
  | QM
  | EQ
  | LESS
  | INF
  | STR of (string)
  | NUM of (float)
  | TU of (int*char)
  | CARD
  | NOT
  | AND
  | OR
  | IMPL
  | EQUIV
  | EX
  | FA
  | PREV
  | NEXT
  | EVENTUALLY
  | ONCE
  | ALWAYS
  | PAST_ALWAYS
  | SINCE
  | UNTIL
  | END
  | EOF

open Parsing;;
# 43 "formula_parser.mly"
  open Predicate
  open MFOTL
  open Misc

  let f str = 
    if Misc.debugging Dbg_formula then
      Printf.printf "[Formula_parser] %s\t\n" str
    else
      ()

(* by default, the time unit is of 1 second *)
  let timeunits (n,c) = 
    let d = 
      match c with
	| 'd' -> 24 * 60 * 60
	| 'h' -> 60 * 60
	| 'm' -> 60
	| 's' -> 1
	| _ -> failwith "[Formula_parser.time_units] unrecognized time unit"
    in  
    float_of_int (d * n)

  let rec exists varlist f =
    match varlist with
      | [] -> failwith "[Formula_parser.exists] no variables"
      | [v] -> Exists (v,f)
      | v::t -> Exists (v, exists t f)

  let rec forall varlist f =
    match varlist with
      | [] -> failwith "[Formula_parser.forall] no variables"
      | [v] -> ForAll (v,f)
      | v::t -> ForAll (v, forall t f)

  let dfintv = (MFOTL.CBnd 0., MFOTL.Inf) 

  let strip str = 
    let len = String.length str in
    if str.[0] = '\"' && str.[len-1] = '\"' then
      String.sub str 1 (len-2)
    else
      str

  let get_cst str = 
    try 
      Int (int_of_string str)
    with _ -> Str (strip str)

  let check f = 
    let _ = 
      match f with
	| Equal (t1,t2)  
	| Less (t1,t2) 
	  -> ( 	
	    match t1,t2 with
	      | Cst (Int _), Cst (Str _) 
	      | Cst (Str _), Cst (Int _) -> 
		failwith "[Formula_parser.check] \
              Comparisons should be between constants of the same type"
	      | _ -> () 
	  )      
	| _ -> failwith "[Formula_parser.check] internal error"
    in f
# 100 "formula_parser.ml"
let yytransl_const = [|
  257 (* LPA *);
  258 (* RPA *);
  259 (* LSB *);
  260 (* RSB *);
  261 (* COM *);
  262 (* DOT *);
  263 (* QM *);
  264 (* EQ *);
  265 (* LESS *);
  266 (* INF *);
  270 (* CARD *);
  271 (* NOT *);
  272 (* AND *);
  273 (* OR *);
  274 (* IMPL *);
  275 (* EQUIV *);
  276 (* EX *);
  277 (* FA *);
  278 (* PREV *);
  279 (* NEXT *);
  280 (* EVENTUALLY *);
  281 (* ONCE *);
  282 (* ALWAYS *);
  283 (* PAST_ALWAYS *);
  284 (* SINCE *);
  285 (* UNTIL *);
  286 (* END *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  267 (* STR *);
  268 (* NUM *);
  269 (* TU *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\005\000\006\000\006\000\007\000\007\000\007\000\007\000\
\008\000\008\000\002\000\009\000\003\000\003\000\003\000\012\000\
\012\000\010\000\010\000\010\000\004\000\004\000\004\000\011\000\
\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\004\000\004\000\003\000\002\000\003\000\002\000\
\003\000\003\000\002\000\003\000\003\000\002\000\004\000\003\000\
\004\000\003\000\002\000\002\000\002\000\002\000\002\000\002\000\
\001\000\001\000\004\000\001\000\001\000\001\000\001\000\003\000\
\001\000\003\000\001\000\000\000\003\000\001\000\000\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\039\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\037\000\038\000\000\000\
\000\000\048\000\000\000\010\000\000\000\046\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\040\000\000\000\000\000\000\000\033\000\027\000\034\000\028\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
\000\000\000\000\000\000\000\000\000\000\000\000\041\000\003\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
\000\000\000\000\000\000\000\000\035\000\031\000\032\000\029\000\
\030\000\042\000"

let yydgoto = "\002\000\
\024\000\019\000\020\000\029\000\035\000\036\000\088\000\062\000\
\021\000\083\000\022\000\023\000"

let yysindex = "\255\255\
\126\255\000\000\126\255\003\255\013\255\000\000\000\000\126\255\
\126\255\022\255\022\255\153\255\153\255\053\255\153\255\053\255\
\153\255\165\255\000\000\043\255\040\255\000\000\000\000\080\255\
\062\255\000\000\165\255\000\000\066\255\000\000\073\255\099\255\
\104\255\071\255\126\255\087\255\071\255\126\255\047\255\047\255\
\126\255\071\255\126\255\126\255\071\255\126\255\126\255\126\255\
\126\255\126\255\153\255\053\255\019\255\019\255\019\255\000\000\
\000\000\126\255\126\255\000\000\000\000\000\000\000\000\000\000\
\071\255\051\255\071\255\071\255\071\255\071\255\071\255\000\000\
\079\255\114\255\114\255\165\255\126\255\126\255\000\000\000\000\
\000\000\096\255\101\255\165\255\000\000\165\255\065\255\000\000\
\132\255\165\255\165\255\019\255\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\049\255\000\000\000\000\
\000\000\112\255\112\255\000\000\000\000\000\000\000\000\000\000\
\000\000\107\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\007\000\000\000\000\000\009\000\000\000\000\000\000\000\
\000\000\011\000\000\000\000\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\133\255\000\000\
\000\000\000\000\000\000\029\255\000\000\000\000\000\000\000\000\
\015\000\000\000\019\000\021\000\023\000\025\000\043\000\000\000\
\001\000\003\000\005\000\004\000\000\000\000\000\000\000\000\000\
\000\000\137\255\000\000\006\000\085\255\008\000\000\000\000\000\
\000\000\010\000\012\000\133\255\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\027\000\000\000\219\255\117\000\032\000\000\000\000\000\243\255\
\000\000\050\000\022\000\000\000"

let yytablesize = 328
let yytable = "\001\000\
\007\000\009\000\006\000\024\000\005\000\011\000\014\000\012\000\
\016\000\023\000\019\000\025\000\022\000\025\000\013\000\080\000\
\081\000\082\000\015\000\064\000\017\000\004\000\018\000\026\000\
\020\000\005\000\064\000\018\000\005\000\079\000\007\000\030\000\
\030\000\034\000\027\000\028\000\039\000\039\000\034\000\037\000\
\055\000\042\000\021\000\045\000\038\000\041\000\043\000\044\000\
\046\000\036\000\053\000\054\000\089\000\039\000\082\000\040\000\
\041\000\041\000\063\000\061\000\087\000\065\000\063\000\061\000\
\067\000\057\000\094\000\068\000\095\000\069\000\070\000\058\000\
\071\000\072\000\073\000\074\000\075\000\076\000\059\000\085\000\
\085\000\056\000\077\000\078\000\084\000\086\000\047\000\048\000\
\049\000\050\000\045\000\066\000\037\000\037\000\047\000\047\000\
\048\000\049\000\050\000\003\000\092\000\004\000\093\000\090\000\
\091\000\005\000\049\000\051\000\052\000\006\000\060\000\061\000\
\008\000\009\000\025\000\063\000\061\000\047\000\010\000\011\000\
\012\000\013\000\014\000\015\000\016\000\017\000\003\000\031\000\
\004\000\047\000\048\000\049\000\005\000\096\000\044\000\097\000\
\006\000\007\000\043\000\008\000\009\000\098\000\000\000\000\000\
\000\000\010\000\011\000\012\000\013\000\014\000\015\000\016\000\
\017\000\032\000\000\000\033\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\006\000\007\000\000\000\008\000\009\000\
\000\000\000\000\000\000\000\000\010\000\011\000\012\000\013\000\
\014\000\015\000\016\000\017\000\047\000\048\000\049\000\050\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\051\000\052\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\000\009\000\006\000\024\000\005\000\011\000\
\014\000\012\000\016\000\023\000\019\000\025\000\022\000\000\000\
\013\000\007\000\007\000\007\000\015\000\006\000\017\000\005\000\
\018\000\000\000\020\000\000\000\007\000\007\000\006\000\006\000\
\005\000\005\000\014\000\014\000\016\000\016\000\019\000\019\000\
\022\000\022\000\013\000\013\000\021\000\000\000\015\000\015\000\
\017\000\017\000\018\000\018\000\020\000\020\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\021\000\021\000"

let yycheck = "\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\001\000\000\053\000\
\054\000\055\000\000\000\033\000\000\000\003\001\000\000\011\001\
\000\000\007\001\040\000\001\000\007\001\011\001\012\001\010\000\
\011\000\005\001\008\000\009\000\008\001\009\001\012\000\013\000\
\001\001\015\000\000\000\017\000\013\000\014\000\015\000\016\000\
\017\000\001\001\008\001\009\001\066\000\001\001\092\000\003\001\
\008\001\009\001\012\001\013\001\010\001\035\000\012\001\013\001\
\038\000\004\001\002\001\041\000\004\001\043\000\044\000\006\001\
\046\000\047\000\048\000\049\000\050\000\051\000\006\001\058\000\
\059\000\002\001\051\000\052\000\058\000\059\000\016\001\017\001\
\018\001\019\001\006\001\005\001\008\001\009\001\016\001\016\001\
\017\001\018\001\019\001\001\001\005\001\003\001\002\001\077\000\
\078\000\007\001\000\000\028\001\029\001\011\001\012\001\013\001\
\014\001\015\001\011\001\012\001\013\001\006\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\001\001\011\000\
\003\001\016\001\017\001\018\001\007\001\002\001\002\001\004\001\
\011\001\012\001\002\001\014\001\015\001\092\000\255\255\255\255\
\255\255\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\001\001\255\255\003\001\255\255\255\255\255\255\007\001\
\255\255\255\255\255\255\011\001\012\001\255\255\014\001\015\001\
\255\255\255\255\255\255\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\016\001\017\001\018\001\019\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\028\001\029\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\002\001\002\001\002\001\002\001\002\001\
\002\001\002\001\002\001\002\001\002\001\002\001\002\001\255\255\
\002\001\017\001\018\001\019\001\002\001\019\001\002\001\019\001\
\002\001\255\255\002\001\255\255\028\001\029\001\028\001\029\001\
\028\001\029\001\028\001\029\001\028\001\029\001\028\001\029\001\
\028\001\029\001\028\001\029\001\002\001\255\255\028\001\029\001\
\028\001\029\001\028\001\029\001\028\001\029\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\028\001\029\001"

let yynames_const = "\
  LPA\000\
  RPA\000\
  LSB\000\
  RSB\000\
  COM\000\
  DOT\000\
  QM\000\
  EQ\000\
  LESS\000\
  INF\000\
  CARD\000\
  NOT\000\
  AND\000\
  OR\000\
  IMPL\000\
  EQUIV\000\
  EX\000\
  FA\000\
  PREV\000\
  NEXT\000\
  EVENTUALLY\000\
  ONCE\000\
  ALWAYS\000\
  PAST_ALWAYS\000\
  SINCE\000\
  UNTIL\000\
  END\000\
  EOF\000\
  "

let yynames_block = "\
  STR\000\
  NUM\000\
  TU\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : MFOTL.formula) in
    Obj.repr(
# 133 "formula_parser.mly"
                                        ( f "f()"; _2 )
# 340 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 134 "formula_parser.mly"
                                        ( f "f(pred)"; Pred _1)
# 347 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 135 "formula_parser.mly"
                                        ( f "f(eq)"; check (Equal (_1,_3)) )
# 355 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 136 "formula_parser.mly"
                                        ( f "f(eq)"; check (Less (_1,_3)) )
# 363 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : MFOTL.formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 137 "formula_parser.mly"
                                        ( f "f(<=>)"; Equiv (_1,_3) )
# 371 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : MFOTL.formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 138 "formula_parser.mly"
                                        ( f "f(=>)"; Implies (_1,_3) )
# 379 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : MFOTL.formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 139 "formula_parser.mly"
                                        ( f "f(or)"; Or (_1,_3) )
# 387 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : MFOTL.formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 140 "formula_parser.mly"
                                        ( f "f(and)"; And (_1,_3) )
# 395 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 141 "formula_parser.mly"
                                        ( f "f(card)"; Card (_2) )
# 402 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 142 "formula_parser.mly"
                                        ( f "f(not)"; Neg (_2) )
# 409 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'varlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 143 "formula_parser.mly"
                                        ( f "f(ex)"; exists _2 _4 )
# 417 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'varlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 144 "formula_parser.mly"
                                        ( f "f(fa)"; forall _2 _4 )
# 425 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'interval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 145 "formula_parser.mly"
                                        ( f "f(prev)"; Prev (_2,_3) )
# 433 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 146 "formula_parser.mly"
                                        ( f "f(prevdf)"; Prev (dfintv,_2) )
# 440 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'interval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 147 "formula_parser.mly"
                                        ( f "f(next)"; Next (_2,_3) )
# 448 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 148 "formula_parser.mly"
                                        ( f "f(nextdf)"; Next (dfintv,_2) )
# 455 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'interval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 149 "formula_parser.mly"
                                        ( f "f(ev)"; Eventually (_2,_3) )
# 463 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'interval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 150 "formula_parser.mly"
                                        ( f "f(once)"; Once (_2,_3) )
# 471 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 151 "formula_parser.mly"
                                        ( f "f(oncedf)"; Once (dfintv,_2) )
# 478 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'interval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 152 "formula_parser.mly"
                                        ( f "f(always)"; Always (_2,_3) )
# 486 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'interval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 153 "formula_parser.mly"
                                        ( f "f(palways)"; PastAlways (_2,_3) )
# 494 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 154 "formula_parser.mly"
                                        ( f "f(palwaysdf)"; PastAlways (dfintv,_2) )
# 501 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : MFOTL.formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'interval) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 155 "formula_parser.mly"
                                        ( f "f(since)"; Since (_3,_1,_4) )
# 510 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : MFOTL.formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 156 "formula_parser.mly"
                                        ( f "f(sincedf)"; Since (dfintv,_1,_3) )
# 518 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : MFOTL.formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'interval) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : MFOTL.formula) in
    Obj.repr(
# 157 "formula_parser.mly"
                                        ( f "f(until)"; Until (_3,_1,_4) )
# 527 "formula_parser.ml"
               : MFOTL.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lbound) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rbound) in
    Obj.repr(
# 161 "formula_parser.mly"
                              ( f "interval"; (_1,_3) )
# 535 "formula_parser.ml"
               : 'interval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'units) in
    Obj.repr(
# 164 "formula_parser.mly"
                              ( f "opened lbound"; OBnd _2 )
# 542 "formula_parser.ml"
               : 'lbound))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'units) in
    Obj.repr(
# 165 "formula_parser.mly"
                              ( f "closed lbound"; CBnd _2 )
# 549 "formula_parser.ml"
               : 'lbound))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'units) in
    Obj.repr(
# 168 "formula_parser.mly"
                              ( f "opened rbound"; OBnd _1 )
# 556 "formula_parser.ml"
               : 'rbound))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'units) in
    Obj.repr(
# 169 "formula_parser.mly"
                              ( f "closed rbound"; CBnd _1 )
# 563 "formula_parser.ml"
               : 'rbound))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "formula_parser.mly"
                              ( f "no bound(1)"; Inf )
# 569 "formula_parser.ml"
               : 'rbound))
; (fun __caml_parser_env ->
    Obj.repr(
# 171 "formula_parser.mly"
                              ( f "no bound(2)"; Inf )
# 575 "formula_parser.ml"
               : 'rbound))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int*char) in
    Obj.repr(
# 174 "formula_parser.mly"
                              ( f "ts";  timeunits _1 )
# 582 "formula_parser.ml"
               : 'units))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 175 "formula_parser.mly"
                              ( f "int"; _1 )
# 589 "formula_parser.ml"
               : 'units))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'pred) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'termlist) in
    Obj.repr(
# 179 "formula_parser.mly"
                                 ( f "p()"; 
				    Predicate.make_predicate (_1,_3) )
# 598 "formula_parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 183 "formula_parser.mly"
                              ( f "pred"; _1 )
# 605 "formula_parser.ml"
               : 'pred))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 187 "formula_parser.mly"
                              ( f "term(var)"; Var _1 )
# 612 "formula_parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'str_sb) in
    Obj.repr(
# 188 "formula_parser.mly"
                              ( f "term(cst.str)"; Cst (get_cst _1) )
# 619 "formula_parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 189 "formula_parser.mly"
                              ( f "term(cst.num)"; Cst (Int (int_of_float _1)) )
# 626 "formula_parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 192 "formula_parser.mly"
                              ( f "str_sb([str])"; "[" ^ _2 ^ "]" )
# 633 "formula_parser.ml"
               : 'str_sb))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 193 "formula_parser.mly"
                              ( f "str_sb(str)"; _1 )
# 640 "formula_parser.ml"
               : 'str_sb))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'termlist) in
    Obj.repr(
# 196 "formula_parser.mly"
                              ( f "termlist(list)"; _1 :: _3 )
# 648 "formula_parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 197 "formula_parser.mly"
                            ( f "termlist(end)"; [_1] )
# 655 "formula_parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 198 "formula_parser.mly"
                        ( f "termlist()"; [] )
# 661 "formula_parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'varlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 201 "formula_parser.mly"
                              ( f "varlist(list)"; _1 @ [_3] )
# 669 "formula_parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 202 "formula_parser.mly"
                           ( f "varlist(end)"; [_1] )
# 676 "formula_parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 203 "formula_parser.mly"
                        ( f "varlist()"; [] )
# 682 "formula_parser.ml"
               : 'varlist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 206 "formula_parser.mly"
                             ( f "var"; _2 )
# 689 "formula_parser.ml"
               : 'var))
(* Entry formula *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let formula (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : MFOTL.formula)
