
exception Not_implemented
exception ElementNotFoundException
exception InvalidStackException
exception InvalidClosureException
exception UnResolvableClosure
exception Invalid_Parameter

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
  | RLambda of ( (string * exptype)* (expr* exptype) * expr) (* fname, (variable of x,type), functionbody*)
and definition =
    Simple of (string* exptype)* expr
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition

and  exptype = Tint | Tbool | Tfunc of (exptype * exptype) | Ttuple of (exptype list) | Tunit


type closure = Clos of (expr * table) | Tclos of (closure list) |NULL
and table = (string * closure )list

type answer  = NumVal of int | BoolVal of bool | TupVal of int * (answer list) | VClos of ((string* expr) * table)

type stackElement = CLOSURE of closure | ADD of closure | MULT of closure | AND of closure | OR of closure | NOT | NEGATIVE | ABS| CMP | IFTE of (closure * closure)
                  | TUP of ( closure list * closure list)| PROJ of ( int * int) | DIV of (closure * closure) | REM of (closure * closure) | EQS of (closure * closure)
                  | GTE of (closure * closure) | LTE of (closure * closure) | GT of (closure * closure) | LT of (closure * closure) |  UNBIND| LET of string * expr * table

                  val makeClosure : table -> expr -> closure
                  val absInt : int -> int
                  val getVarFromTable : string -> (string * 'b) list -> 'b
                  val getExp : closure -> table
                  val runKrivine : closure -> stackElement list -> closure
                  val resolveClosure : closure -> answer
                  val getAnswer : expr -> table -> answer
