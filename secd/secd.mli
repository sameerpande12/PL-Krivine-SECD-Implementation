open A0

type expr =
    V of string
  | Bool of bool
  | Integer of int
  (**)
  | Lambda of ((expr * exptype) * expr)
  | App of (expr * expr)
  (**)
  | Plus of (expr * expr)
  | Mult of (expr * expr)
  | Sub of (expr* expr)
  | Div of (expr* expr)
  | Rem of (expr* expr)
  (**)
  | And of (expr * expr)
  | Or of (expr * expr)
  (**)
  | Not of (expr)
  | Negative of (expr)
  | Abs of (expr)
  (**)
  | Cmp of expr
  | Equals of expr * expr      (* = *)
  | GreaterTE of expr * expr   (* >= *)
  | LessTE of expr * expr      (* <= *)
  | GreaterT of expr * expr    (* > *)
  | LessT of expr * expr       (* < *)
  (**)
  | InParen of expr               (* ( ) *)
  (**)
  | If_Then_Else of (expr * expr * expr)
  (**)
  | Tuple of int*(expr list)
  (**)
  | Project of (int*int) * (expr)
  | Let of definition * expr
  | RLambda of ( (string * exptype) * (expr* exptype) * expr) (* fname, (variable of x,type), functionbody*)
and definition =
    Simple of (string* exptype)* expr
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition

and  exptype = Tint | Tbool | Tfunc of (exptype * exptype) | Ttuple of (exptype list) | Tunit



type answer = B of bool | Num of bigint | Tup of int * (answer list) | VClos of ( ( string* (opcode list)) * table ) | RClos of (string* ( string* (opcode list)) * table ) (*| RClos of (string* (string* (opcode list)* table)) *)
and table = (string * answer) list
and opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
           | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT | CMP
           | PAREN | COND of (opcode list * opcode list) | TUPLE of int | PROJ of int*int | LET  | FABS of string * (opcode list) | FCALL | RETURN | BIND of (string * exptype)
           | SIMPLEDEF of string * (opcode list) | SEQCOMPOSE | PARCOMPOSE | LOCALDEF| RABS of string * (string * (opcode list))


val compile : expr -> opcode list
val getFirstn : 'a list -> int -> 'a list
val removeFirstn : 'a list -> int -> 'a list
val getFromTable : 'a -> ('a * 'b) list -> 'b
val execute :
  answer list ->
  table -> opcode list -> (answer list * table * opcode list) list -> answer
val getAnswer: expr -> table -> answer
