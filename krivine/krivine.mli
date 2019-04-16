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
  | Lambda of (expr * expr)
  | App of (expr * expr)
  | Plus of (expr * expr)
  | Mult of (expr * expr)
  | Minus of (expr * expr)
  | And of (expr * expr)
  | Or of (expr * expr)
  | Not of expr
  | Negative of expr
  | Abs of expr
  | Cmp of expr
  | If_Then_Else of (expr * expr * expr)
  | Tuple of int * expr list
  | Project of (int * int) * expr
type exptype =
    Tint
  | Tbool
  | Tfunc of (exptype * exptype)
  | Ttuple of exptype list
  | Tunit
type closure = Clos of (expr * table) | Tclos of closure list
and table = (string * closure) list
type answer =
    NumVal of int
  | BoolVal of bool
  | TupVal of int * answer list
  | VClos of ((string * expr) * table)
type stackElement =
    CLOSURE of closure
  | ADD of closure
  | MULT of closure
  | AND of closure
  | OR of closure
  | NOT
  | NEGATIVE
  | ABS
  | CMP
  | IFTE of (closure * closure)
  | TUP of (closure list * closure list)
  | PROJ of (int * int)
val makeClosure : table -> expr -> closure
val absInt : int -> int
val getVarFromTable : 'a -> ('a * 'b) list -> 'b
val getExp : closure -> table
val runKrivine : closure -> stackElement list -> closure
val resolveClosure : closure -> answer
val getAnswer : expr -> table -> answer
