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

val type_parser :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Secd.exptype
val def_parser :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Secd.definition
val exp_parser :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Secd.expr
