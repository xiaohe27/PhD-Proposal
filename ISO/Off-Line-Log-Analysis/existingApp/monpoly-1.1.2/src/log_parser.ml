type token =
  | AT
  | LPA
  | RPA
  | COM
  | STR of (string)
  | EOF

open Parsing;;
# 43 "log_parser.mly"
  open Misc
  open Predicate
  open MFOTL
  open Db
  open Filter_rel

  let f str = 
    if Misc.debugging Dbg_log then
      Printf.printf "[Log_parser] %s with start=%d and end=%d\n%!" str (symbol_start()) (symbol_end())
    else
      ()

  let preds = ref []

  let update_preds l = 
    preds := l;
    l

  let get_type = function
    | "int" -> TInt
    | "string" -> TStr
    | t -> let spos = Parsing.symbol_start_pos() in
	   let str = Printf.sprintf 
	     "[Log_parser.check_fields] Unknown type %s in signature at line %d."
	     t spos.Lexing.pos_lnum
	   in
	   failwith str

  let make_predicate p attr =
    let tl = 
      List.map 
	(fun str -> 
	  match Str.split (Str.regexp ":") str with
	    | [] -> failwith "[Log_parser.make_predicate] internal error"
	    | [type_str] -> "", get_type type_str
	    | var_name :: type_str :: _ -> 	      
	      var_name, get_type type_str
	)
	attr
    in
    (p, tl)


  let get_schema pname =  
    try
      List.find (fun (p, _) -> pname = p) !preds
    with Not_found -> 
      let spos = Parsing.symbol_start_pos() in
      let str = Printf.sprintf 
	"[Log_parser.get_schema] The predicate %s at line %d was not found in the signature." 
	pname spos.Lexing.pos_lnum
      in
      failwith str



  let process_tuple pname attr ar t = 
    if List.length t = ar then 
      try
	Tuple.make_tuple2 t attr
      with Failure "int_of_string" -> 
	let str = Printf.sprintf 
	  "[Log_parser.make_tuple] Wrong type for tuple field for predicate %s at line %d in the log file." 
	  pname (Parsing.symbol_start_pos()).Lexing.pos_lnum
	in
	failwith str
    else 
      let str = Printf.sprintf 
	"[Log_parser.make_tuple] Wrong tuple length for predicate %s at line %d in the log file." 
	pname (Parsing.symbol_start_pos()).Lexing.pos_lnum
      in
      failwith str

  let process_tuples s tuples = 
    let pname, attr = s in
    let ar = List.length attr in
    (* we only reverse because [rev_map] is tail-recursive, while [map] is not *)
    List.rev_map (process_tuple pname attr ar) tuples
	 
  (* a tuple is a list of strings here, not a value of type Tuple.tuple *)
  let make_table p tuples = 
    let s = get_schema p in
    let rel =
      if !Filter_rel.enabled then
	if Filter_rel.rel_OK p then
	  List.filter (Filter_rel.tuple_OK p) (process_tuples s tuples)
	else
	  []
    else
      process_tuples s tuples
    in
    s, (Relation.make_relation rel)



  (* db is seen here as an association list *) 
  let add_table db (s,rel) =
    if Relation.is_empty rel then
      db
    else if List.mem_assoc s db then
      let rel' = List.assoc s db in
      let new_rel = Relation.union rel rel' in
	(s,new_rel)::(List.remove_assoc s db)
    else
      (s,rel)::db

  let make_db db = 
     Db.make_db (List.map (fun (s,r) -> Table.make_table s r) db)

# 121 "log_parser.ml"
let yytransl_const = [|
  257 (* AT *);
  258 (* LPA *);
  259 (* RPA *);
  260 (* COM *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  261 (* STR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\002\000\002\000\002\000\002\000\005\000\
\005\000\006\000\007\000\007\000\008\000\004\000\004\000\004\000\
\000\000\000\000"

let yylen = "\002\000\
\002\000\000\000\004\000\004\000\004\000\002\000\001\000\002\000\
\000\000\002\000\002\000\000\000\003\000\003\000\001\000\000\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\017\000\000\000\000\000\007\000\
\018\000\000\000\001\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\000\000\000\010\000\000\000\004\000\
\005\000\008\000\014\000\000\000\011\000\013\000"

let yydgoto = "\003\000\
\005\000\009\000\006\000\015\000\017\000\018\000\022\000\023\000"

let yysindex = "\008\000\
\251\254\003\000\000\000\003\255\000\000\251\254\002\000\000\000\
\000\000\006\255\000\000\007\255\000\000\009\255\011\255\013\255\
\004\000\007\255\006\255\000\000\006\255\000\000\013\255\000\000\
\000\000\000\000\000\000\014\255\000\000\000\000"

let yyrindex = "\000\000\
\016\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\
\000\000\015\255\000\000\007\000\000\000\016\255\000\000\001\000\
\000\000\007\000\015\255\000\000\015\255\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\014\000\000\000\000\000\243\255\005\000\000\000\254\255\000\000"

let yytablesize = 264
let yytable = "\004\000\
\012\000\013\000\008\000\025\000\010\000\027\000\009\000\028\000\
\001\000\002\000\014\000\016\000\019\000\020\000\021\000\002\000\
\030\000\016\000\015\000\011\000\029\000\000\000\026\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\000\000\007\000\024\000\012\000\012\000\009\000"

let yycheck = "\005\001\
\000\000\000\000\000\000\000\000\002\001\019\000\000\000\021\000\
\001\000\002\000\005\001\005\001\004\001\003\001\002\001\000\000\
\003\001\003\001\003\001\006\000\023\000\255\255\018\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\001\001\001\001\005\001\005\001\001\001"

let yynames_const = "\
  AT\000\
  LPA\000\
  RPA\000\
  COM\000\
  EOF\000\
  "

let yynames_block = "\
  STR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'predicate) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (Db.schema)) in
    Obj.repr(
# 172 "log_parser.mly"
                                ( f "signature(list)"; update_preds (_1::_2) )
# 259 "log_parser.ml"
               : (Db.schema)))
; (fun __caml_parser_env ->
    Obj.repr(
# 173 "log_parser.mly"
                                ( f "signature(end)"; update_preds [] )
# 265 "log_parser.ml"
               : (Db.schema)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'fields) in
    Obj.repr(
# 176 "log_parser.mly"
                                ( f "predicate"; make_predicate _1 _3 )
# 273 "log_parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'db) in
    Obj.repr(
# 182 "log_parser.mly"
                                ( f "tsdb(next)"; Some (MFOTL.ts_of_string "Log_parser" _2, make_db _3) )
# 281 "log_parser.ml"
               : (MFOTL.timestamp * Db.db) option))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'db) in
    Obj.repr(
# 183 "log_parser.mly"
                                ( f "tsdb(last)"; Some (MFOTL.ts_of_string "Log_parser" _2, make_db _3) )
# 289 "log_parser.ml"
               : (MFOTL.timestamp * Db.db) option))
; (fun __caml_parser_env ->
    Obj.repr(
# 184 "log_parser.mly"
                                ( f "tsdb(ts eof)"; None )
# 295 "log_parser.ml"
               : (MFOTL.timestamp * Db.db) option))
; (fun __caml_parser_env ->
    Obj.repr(
# 185 "log_parser.mly"
                                ( f "tsdb(eof)"; None )
# 301 "log_parser.ml"
               : (MFOTL.timestamp * Db.db) option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'table) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'db) in
    Obj.repr(
# 188 "log_parser.mly"
                                ( f "db(list)"; add_table _2 _1 )
# 309 "log_parser.ml"
               : 'db))
; (fun __caml_parser_env ->
    Obj.repr(
# 189 "log_parser.mly"
                                ( f "db()"; [] )
# 315 "log_parser.ml"
               : 'db))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'relation) in
    Obj.repr(
# 192 "log_parser.mly"
                                ( f "table"; make_table _1 _2 )
# 323 "log_parser.ml"
               : 'table))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tuple) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'relation) in
    Obj.repr(
# 195 "log_parser.mly"
                                ( f "relation(list)"; _1::_2 )
# 331 "log_parser.ml"
               : 'relation))
; (fun __caml_parser_env ->
    Obj.repr(
# 196 "log_parser.mly"
                                ( f "relation(end)"; [] )
# 337 "log_parser.ml"
               : 'relation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fields) in
    Obj.repr(
# 199 "log_parser.mly"
                                ( f "tuple"; _2 )
# 344 "log_parser.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 203 "log_parser.mly"
                               ( f "fields(list)"; _1::_3 )
# 352 "log_parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 204 "log_parser.mly"
                             ( f "fields(end)"; [_1] )
# 359 "log_parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    Obj.repr(
# 205 "log_parser.mly"
                          ( f "fields()"; [] )
# 365 "log_parser.ml"
               : 'fields))
(* Entry signature *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry tsdb *)
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
let signature (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (Db.schema))
let tsdb (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : (MFOTL.timestamp * Db.db) option)
