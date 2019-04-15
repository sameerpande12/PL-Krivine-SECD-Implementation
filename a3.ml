type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | ABS
  | TILDA
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | REM
  | CONJ
  | DISJ
  | EQ
  | GT
  | LT
  | LP
  | RP
  | IF
  | THEN
  | ELSE
  | FI
  | COMMA
  | PROJ
  | CMP
  | LET
  | IN
  | END
  | BACKSLASH
  | DOT
  | DEF
  | SEMICOLON
  | PARALLEL
  | LOCAL
  | EOF
  | COLON
  | ITYPE
  | BTYPE
  | UTYPE
  | TUPTYPE
  | FUNCTYPE

open Parsing;;
let _ = parse_error;;
# 2 "a3.mly"
    open Secd
# 49 "a3.ml"
let yytransl_const = [|
  260 (* ABS *);
  261 (* TILDA *);
  262 (* NOT *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIV *);
  267 (* REM *);
  268 (* CONJ *);
  269 (* DISJ *);
  270 (* EQ *);
  271 (* GT *);
  272 (* LT *);
  273 (* LP *);
  274 (* RP *);
  275 (* IF *);
  276 (* THEN *);
  277 (* ELSE *);
  278 (* FI *);
  279 (* COMMA *);
  280 (* PROJ *);
  281 (* CMP *);
  282 (* LET *);
  283 (* IN *);
  284 (* END *);
  285 (* BACKSLASH *);
  286 (* DOT *);
  287 (* DEF *);
  288 (* SEMICOLON *);
  289 (* PARALLEL *);
  290 (* LOCAL *);
    0 (* EOF *);
  291 (* COLON *);
  292 (* ITYPE *);
  293 (* BTYPE *);
  294 (* UTYPE *);
  295 (* TUPTYPE *);
  296 (* FUNCTYPE *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\003\000\004\000\004\000\005\000\005\000\006\000\006\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\008\000\008\000\
\008\000\009\000\009\000\009\000\009\000\010\000\010\000\010\000\
\011\000\011\000\011\000\011\000\013\000\013\000\015\000\015\000\
\016\000\016\000\017\000\017\000\018\000\018\000\019\000\019\000\
\019\000\002\000\014\000\020\000\020\000\021\000\001\000\012\000\
\012\000\023\000\023\000\022\000\022\000\024\000\024\000\025\000\
\025\000\025\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\003\000\001\000\002\000\001\000\002\000\
\003\000\003\000\004\000\003\000\004\000\001\000\003\000\003\000\
\001\000\003\000\003\000\003\000\001\000\002\000\002\000\001\000\
\006\000\004\000\005\000\001\000\007\000\001\000\007\000\001\000\
\003\000\001\000\003\000\003\000\003\000\001\000\001\000\001\000\
\001\000\002\000\001\000\003\000\001\000\006\000\002\000\004\000\
\001\000\003\000\003\000\003\000\001\000\003\000\001\000\001\000\
\001\000\001\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\056\000\057\000\058\000\
\059\000\000\000\049\000\053\000\055\000\000\000\000\000\060\000\
\000\000\043\000\045\000\040\000\041\000\039\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\061\000\
\000\000\000\000\005\000\000\000\000\000\000\000\021\000\000\000\
\028\000\030\000\032\000\034\000\038\000\000\000\000\000\000\000\
\047\000\000\000\000\000\042\000\022\000\023\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\054\000\052\000\000\000\044\000\000\000\
\037\000\000\000\033\000\000\000\000\000\000\000\000\000\000\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\019\000\018\000\000\000\000\000\051\000\048\000\000\000\
\000\000\036\000\000\000\000\000\000\000\000\000\000\000\000\000\
\026\000\000\000\000\000\000\000\027\000\000\000\000\000\000\000\
\000\000\025\000\029\000\031\000"

let yydgoto = "\004\000\
\009\000\016\000\032\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\010\000\041\000\017\000\042\000\043\000\
\057\000\044\000\045\000\018\000\019\000\011\000\047\000\012\000\
\013\000"

let yysindex = "\094\000\
\002\255\250\254\075\255\000\000\002\255\000\000\000\000\000\000\
\000\000\012\000\000\000\000\000\000\000\250\254\029\255\000\000\
\016\000\000\000\000\000\000\000\000\000\000\000\139\255\139\255\
\075\255\075\255\075\255\028\255\139\255\250\254\044\255\000\000\
\005\000\039\255\000\000\131\255\065\255\140\255\000\000\048\255\
\000\000\000\000\000\000\000\000\000\000\015\255\043\255\052\255\
\000\000\045\255\054\255\000\000\000\000\000\000\000\000\070\255\
\069\255\033\255\097\255\065\255\083\255\067\255\075\255\000\000\
\075\255\139\255\104\255\110\255\139\255\139\255\139\255\139\255\
\139\255\075\255\002\255\000\000\000\000\002\255\000\000\002\255\
\000\000\075\255\000\000\075\255\096\255\075\255\002\255\039\255\
\000\000\065\255\139\255\065\255\139\255\065\255\140\255\140\255\
\000\000\000\000\000\000\046\255\076\255\000\000\000\000\013\255\
\021\255\000\000\007\255\121\255\009\255\000\255\065\255\065\255\
\000\000\075\255\075\255\107\255\000\000\012\255\113\255\035\255\
\012\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\194\000\000\000\183\000\069\000\018\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\086\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\205\000\
\000\000\103\000\000\000\120\000\000\000\137\000\035\000\052\000\
\000\000\000\000\000\000\000\000\114\255\000\000\000\000\000\000\
\117\255\000\000\000\000\000\000\000\000\000\000\154\000\171\000\
\000\000\000\000\000\000\000\000\000\000\000\000\017\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\232\255\068\000\240\255\000\000\231\255\
\047\000\239\255\000\000\251\255\209\255\252\255\000\000\000\000\
\056\000\000\000\000\000\000\000\000\000\070\000\077\000\000\000\
\000\000"

let yytablesize = 489
let yytable = "\046\000\
\024\000\056\000\058\000\060\000\064\000\053\000\054\000\048\000\
\055\000\050\000\014\000\049\000\020\000\021\000\022\000\052\000\
\046\000\017\000\005\000\063\000\048\000\063\000\048\000\075\000\
\015\000\061\000\114\000\115\000\026\000\118\000\027\000\051\000\
\076\000\063\000\016\000\028\000\117\000\006\000\007\000\008\000\
\090\000\092\000\094\000\082\000\059\000\063\000\062\000\063\000\
\089\000\100\000\065\000\015\000\084\000\097\000\098\000\099\000\
\123\000\105\000\063\000\107\000\077\000\109\000\079\000\113\000\
\074\000\111\000\078\000\112\000\014\000\101\000\122\000\069\000\
\070\000\124\000\104\000\020\000\021\000\022\000\023\000\024\000\
\025\000\110\000\063\000\048\000\075\000\008\000\083\000\081\000\
\080\000\119\000\120\000\026\000\082\000\027\000\001\000\002\000\
\003\000\085\000\028\000\029\000\030\000\087\000\009\000\031\000\
\020\000\021\000\022\000\023\000\024\000\086\000\020\000\021\000\
\022\000\023\000\024\000\095\000\096\000\091\000\108\000\012\000\
\026\000\116\000\027\000\093\000\121\000\063\000\026\000\028\000\
\027\000\030\000\088\000\050\000\031\000\028\000\035\000\030\000\
\010\000\106\000\031\000\020\000\021\000\022\000\023\000\024\000\
\066\000\067\000\068\000\103\000\071\000\072\000\073\000\102\000\
\000\000\013\000\000\000\026\000\000\000\027\000\000\000\000\000\
\000\000\000\000\028\000\000\000\030\000\000\000\000\000\031\000\
\000\000\000\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
\024\000\063\000\024\000\048\000\024\000\024\000\024\000\024\000\
\017\000\017\000\000\000\024\000\024\000\017\000\017\000\017\000\
\017\000\017\000\046\000\017\000\000\000\017\000\017\000\017\000\
\017\000\016\000\016\000\046\000\017\000\017\000\016\000\016\000\
\016\000\016\000\016\000\000\000\016\000\000\000\016\000\016\000\
\016\000\016\000\015\000\015\000\000\000\016\000\016\000\015\000\
\015\000\015\000\015\000\015\000\000\000\015\000\000\000\015\000\
\015\000\015\000\015\000\000\000\000\000\000\000\015\000\015\000\
\014\000\014\000\014\000\014\000\014\000\000\000\014\000\000\000\
\014\000\014\000\014\000\014\000\000\000\000\000\000\000\014\000\
\014\000\008\000\008\000\008\000\008\000\008\000\000\000\008\000\
\000\000\008\000\008\000\008\000\008\000\000\000\000\000\000\000\
\008\000\008\000\009\000\009\000\009\000\009\000\009\000\000\000\
\009\000\000\000\009\000\009\000\009\000\009\000\000\000\000\000\
\000\000\009\000\009\000\012\000\012\000\012\000\012\000\012\000\
\000\000\012\000\000\000\012\000\012\000\012\000\012\000\000\000\
\000\000\000\000\012\000\012\000\010\000\010\000\010\000\010\000\
\010\000\000\000\010\000\000\000\010\000\010\000\010\000\010\000\
\000\000\000\000\000\000\010\000\010\000\013\000\013\000\013\000\
\013\000\013\000\000\000\013\000\000\000\013\000\013\000\013\000\
\013\000\000\000\000\000\000\000\013\000\013\000\011\000\011\000\
\011\000\011\000\011\000\000\000\011\000\000\000\011\000\011\000\
\011\000\011\000\007\000\007\000\000\000\011\000\011\000\000\000\
\007\000\000\000\007\000\007\000\007\000\007\000\003\000\000\000\
\000\000\007\000\007\000\003\000\000\000\003\000\003\000\003\000\
\003\000\002\000\000\000\000\000\003\000\003\000\002\000\000\000\
\002\000\002\000\002\000\002\000\000\000\000\000\000\000\002\000\
\002\000"

let yycheck = "\005\000\
\000\000\026\000\027\000\029\000\000\000\023\000\024\000\008\001\
\025\000\014\000\017\001\000\000\001\001\002\001\003\001\000\000\
\000\000\000\000\017\001\013\001\008\001\013\001\008\001\009\001\
\031\001\030\000\014\001\021\001\017\001\030\001\019\001\003\001\
\018\001\013\001\000\000\024\001\028\001\036\001\037\001\038\001\
\066\000\067\000\068\000\023\001\017\001\013\001\003\001\013\001\
\065\000\074\000\012\001\000\000\020\001\071\000\072\000\073\000\
\022\001\082\000\013\001\084\000\018\001\086\000\018\001\018\001\
\017\001\091\000\015\001\093\000\000\000\075\000\118\000\007\001\
\008\001\121\000\080\000\001\001\002\001\003\001\004\001\005\001\
\006\001\087\000\013\001\008\001\009\001\000\000\018\001\018\001\
\035\001\114\000\115\000\017\001\023\001\019\001\001\000\002\000\
\003\000\001\001\024\001\025\001\026\001\035\001\000\000\029\001\
\001\001\002\001\003\001\004\001\005\001\027\001\001\001\002\001\
\003\001\004\001\005\001\069\000\070\000\014\001\023\001\000\000\
\017\001\001\001\019\001\014\001\018\001\013\001\017\001\024\001\
\019\001\026\001\063\000\018\001\029\001\024\001\018\001\026\001\
\000\000\082\000\029\001\001\001\002\001\003\001\004\001\005\001\
\014\001\015\001\016\001\078\000\009\001\010\001\011\001\075\000\
\255\255\000\000\255\255\017\001\255\255\019\001\255\255\255\255\
\255\255\255\255\024\001\255\255\026\001\255\255\255\255\029\001\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\013\001\018\001\008\001\020\001\021\001\022\001\023\001\
\007\001\008\001\255\255\027\001\028\001\012\001\013\001\014\001\
\015\001\016\001\018\001\018\001\255\255\020\001\021\001\022\001\
\023\001\007\001\008\001\027\001\027\001\028\001\012\001\013\001\
\014\001\015\001\016\001\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\007\001\008\001\255\255\027\001\028\001\012\001\
\013\001\014\001\015\001\016\001\255\255\018\001\255\255\020\001\
\021\001\022\001\023\001\255\255\255\255\255\255\027\001\028\001\
\012\001\013\001\014\001\015\001\016\001\255\255\018\001\255\255\
\020\001\021\001\022\001\023\001\255\255\255\255\255\255\027\001\
\028\001\012\001\013\001\014\001\015\001\016\001\255\255\018\001\
\255\255\020\001\021\001\022\001\023\001\255\255\255\255\255\255\
\027\001\028\001\012\001\013\001\014\001\015\001\016\001\255\255\
\018\001\255\255\020\001\021\001\022\001\023\001\255\255\255\255\
\255\255\027\001\028\001\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\255\255\
\255\255\255\255\027\001\028\001\012\001\013\001\014\001\015\001\
\016\001\255\255\018\001\255\255\020\001\021\001\022\001\023\001\
\255\255\255\255\255\255\027\001\028\001\012\001\013\001\014\001\
\015\001\016\001\255\255\018\001\255\255\020\001\021\001\022\001\
\023\001\255\255\255\255\255\255\027\001\028\001\012\001\013\001\
\014\001\015\001\016\001\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\012\001\013\001\255\255\027\001\028\001\255\255\
\018\001\255\255\020\001\021\001\022\001\023\001\013\001\255\255\
\255\255\027\001\028\001\018\001\255\255\020\001\021\001\022\001\
\023\001\013\001\255\255\255\255\027\001\028\001\018\001\255\255\
\020\001\021\001\022\001\023\001\255\255\255\255\255\255\027\001\
\028\001"

let yynames_const = "\
  ABS\000\
  TILDA\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  REM\000\
  CONJ\000\
  DISJ\000\
  EQ\000\
  GT\000\
  LT\000\
  LP\000\
  RP\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  COMMA\000\
  PROJ\000\
  CMP\000\
  LET\000\
  IN\000\
  END\000\
  BACKSLASH\000\
  DOT\000\
  DEF\000\
  SEMICOLON\000\
  PARALLEL\000\
  LOCAL\000\
  EOF\000\
  COLON\000\
  ITYPE\000\
  BTYPE\000\
  UTYPE\000\
  TUPTYPE\000\
  FUNCTYPE\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'or_expression) in
    Obj.repr(
# 29 "a3.mly"
                     ( _1 )
# 366 "a3.ml"
               : Secd.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'or_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_expression) in
    Obj.repr(
# 33 "a3.mly"
                                                         (Or(_1,_3))
# 374 "a3.ml"
               : 'or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_expression) in
    Obj.repr(
# 34 "a3.mly"
                                                          (_1)
# 381 "a3.ml"
               : 'or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'not_expression) in
    Obj.repr(
# 37 "a3.mly"
                                                       (And(_1,_3))
# 389 "a3.ml"
               : 'and_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'not_expression) in
    Obj.repr(
# 38 "a3.mly"
                                                       (_1)
# 396 "a3.ml"
               : 'and_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'not_expression) in
    Obj.repr(
# 41 "a3.mly"
                     (Not(_2))
# 403 "a3.ml"
               : 'not_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compare_expression) in
    Obj.repr(
# 42 "a3.mly"
                       (_1)
# 410 "a3.ml"
               : 'not_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sum_expression) in
    Obj.repr(
# 45 "a3.mly"
                             (Cmp(_2))
# 417 "a3.ml"
               : 'compare_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compare_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sum_expression) in
    Obj.repr(
# 46 "a3.mly"
                                         ( Equals(_1,_3))
# 425 "a3.ml"
               : 'compare_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compare_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sum_expression) in
    Obj.repr(
# 47 "a3.mly"
                                         ( LessT(_1,_3))
# 433 "a3.ml"
               : 'compare_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'compare_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'sum_expression) in
    Obj.repr(
# 48 "a3.mly"
                                            (LessTE(_1,_4))
# 441 "a3.ml"
               : 'compare_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compare_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sum_expression) in
    Obj.repr(
# 49 "a3.mly"
                                         ( GreaterT(_1,_3))
# 449 "a3.ml"
               : 'compare_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'compare_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'sum_expression) in
    Obj.repr(
# 50 "a3.mly"
                                            (GreaterTE(_1,_4))
# 457 "a3.ml"
               : 'compare_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sum_expression) in
    Obj.repr(
# 51 "a3.mly"
                   (_1)
# 464 "a3.ml"
               : 'compare_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sum_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'muldivrem_expression) in
    Obj.repr(
# 54 "a3.mly"
                                            ( Sub(_1,_3))
# 472 "a3.ml"
               : 'sum_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sum_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'muldivrem_expression) in
    Obj.repr(
# 55 "a3.mly"
                                             ( Plus(_1,_3))
# 480 "a3.ml"
               : 'sum_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'muldivrem_expression) in
    Obj.repr(
# 56 "a3.mly"
                         (_1)
# 487 "a3.ml"
               : 'sum_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'muldivrem_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs_negative_expression) in
    Obj.repr(
# 59 "a3.mly"
                                                   (Rem(_1,_3))
# 495 "a3.ml"
               : 'muldivrem_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'muldivrem_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs_negative_expression) in
    Obj.repr(
# 60 "a3.mly"
                                                    (Div(_1,_3))
# 503 "a3.ml"
               : 'muldivrem_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'muldivrem_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs_negative_expression) in
    Obj.repr(
# 61 "a3.mly"
                                                      (Mult(_1,_3))
# 511 "a3.ml"
               : 'muldivrem_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'abs_negative_expression) in
    Obj.repr(
# 62 "a3.mly"
                             (_1)
# 518 "a3.ml"
               : 'muldivrem_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'abs_negative_expression) in
    Obj.repr(
# 65 "a3.mly"
                             (Abs(_2))
# 525 "a3.ml"
               : 'abs_negative_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'abs_negative_expression) in
    Obj.repr(
# 66 "a3.mly"
                                (Negative(_2))
# 532 "a3.ml"
               : 'abs_negative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func_expression) in
    Obj.repr(
# 67 "a3.mly"
                   (_1)
# 539 "a3.ml"
               : 'abs_negative_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'types) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'ifte_expression) in
    Obj.repr(
# 71 "a3.mly"
                                                (Lambda((V(_2),_4),_6))
# 548 "a3.ml"
               : 'func_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'func_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'or_expression) in
    Obj.repr(
# 72 "a3.mly"
                                        (App(_1,_3))
# 556 "a3.ml"
               : 'func_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'defns) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'or_expression) in
    Obj.repr(
# 73 "a3.mly"
                                   (Let(_2,_4))
# 564 "a3.ml"
               : 'func_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ifte_expression) in
    Obj.repr(
# 74 "a3.mly"
                    (_1)
# 571 "a3.ml"
               : 'func_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'or_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'or_expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'or_expression) in
    Obj.repr(
# 79 "a3.mly"
                                                            (If_Then_Else(_2,_4,_6))
# 580 "a3.ml"
               : 'ifte_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proj_expression) in
    Obj.repr(
# 80 "a3.mly"
                    (_1)
# 587 "a3.ml"
               : 'ifte_expression))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'ifte_expression) in
    Obj.repr(
# 85 "a3.mly"
                                          ( Project((_3,_5),_7))
# 596 "a3.ml"
               : 'proj_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_expression) in
    Obj.repr(
# 86 "a3.mly"
                    (_1)
# 603 "a3.ml"
               : 'proj_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tuple_list) in
    Obj.repr(
# 90 "a3.mly"
                   ( Tuple( List.length (_2), _2) )
# 610 "a3.ml"
               : 'tuple_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'paren_expression) in
    Obj.repr(
# 91 "a3.mly"
                     ( _1)
# 617 "a3.ml"
               : 'tuple_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'or_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'or_expression) in
    Obj.repr(
# 95 "a3.mly"
                                    ( ((_1)::[(_3)]) )
# 625 "a3.ml"
               : 'tuple_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'or_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_list) in
    Obj.repr(
# 96 "a3.mly"
                                    ( (_1)::(_3))
# 633 "a3.ml"
               : 'tuple_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'or_expression) in
    Obj.repr(
# 101 "a3.mly"
                      ( InParen(_2))
# 640 "a3.ml"
               : 'paren_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 102 "a3.mly"
             (_1)
# 647 "a3.ml"
               : 'paren_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "a3.mly"
   (V(_1))
# 654 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 106 "a3.mly"
      (Integer(_1))
# 661 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 107 "a3.mly"
       (Bool(_1))
# 668 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'defns) in
    Obj.repr(
# 114 "a3.mly"
            (_1)
# 675 "a3.ml"
               : Secd.definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parenDef) in
    Obj.repr(
# 132 "a3.mly"
                                (_1)
# 682 "a3.ml"
               : 'defns))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'defns) in
    Obj.repr(
# 135 "a3.mly"
                       (_2)
# 689 "a3.ml"
               : 'parenDef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simpleDef) in
    Obj.repr(
# 136 "a3.mly"
                       (_1)
# 696 "a3.ml"
               : 'parenDef))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'types) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'or_expression) in
    Obj.repr(
# 139 "a3.mly"
                                          (Simple((_2,_4),_6))
# 705 "a3.ml"
               : 'simpleDef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'types) in
    Obj.repr(
# 144 "a3.mly"
           (_1)
# 712 "a3.ml"
               : Secd.exptype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'tupType) in
    Obj.repr(
# 149 "a3.mly"
                            (Tfunc(_1,_4))
# 720 "a3.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tupType) in
    Obj.repr(
# 150 "a3.mly"
             (_1)
# 727 "a3.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'types) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 154 "a3.mly"
                                   ((_1)::[_3])
# 735 "a3.ml"
               : 'type_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'types) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_list) in
    Obj.repr(
# 155 "a3.mly"
                           ( (_1)::(_3) )
# 743 "a3.ml"
               : 'type_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'type_list) in
    Obj.repr(
# 159 "a3.mly"
                 (Ttuple(_2))
# 750 "a3.ml"
               : 'tupType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parenType) in
    Obj.repr(
# 160 "a3.mly"
               (_1)
# 757 "a3.ml"
               : 'tupType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'types) in
    Obj.repr(
# 163 "a3.mly"
               (_2)
# 764 "a3.ml"
               : 'parenType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simpleType) in
    Obj.repr(
# 164 "a3.mly"
               (_1)
# 771 "a3.ml"
               : 'parenType))
; (fun __caml_parser_env ->
    Obj.repr(
# 168 "a3.mly"
            (Tint)
# 777 "a3.ml"
               : 'simpleType))
; (fun __caml_parser_env ->
    Obj.repr(
# 169 "a3.mly"
              (Tbool)
# 783 "a3.ml"
               : 'simpleType))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "a3.mly"
              (Tunit)
# 789 "a3.ml"
               : 'simpleType))
(* Entry type_parser *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry def_parser *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry exp_parser *)
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
let type_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Secd.exptype)
let def_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Secd.definition)
let exp_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Secd.expr)
