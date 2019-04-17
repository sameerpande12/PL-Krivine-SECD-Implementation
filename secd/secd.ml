open A0
exception InvalidStackException
exception Invalid_Parameter
exception Not_implemented
exception VariableNotFound
exception MachineStuck

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
  | RLambda of ( (string* exptype) * (expr* exptype) * expr) (* fname, (variable of x,type), functionbody*)
and definition =
    Simple of (string* exptype)* expr
    | Sequence of (definition list)
    | Parallel of (definition list)
    | Local of definition * definition

and  exptype = Tint | Tbool | Tfunc of (exptype * exptype) | Ttuple of (exptype list) | Tunit



type answer = B of bool | Num of bigint | Tup of int * (answer list) | VClos of ( ( string* (opcode list)) * table ) | RClos of ( string *( string* (opcode list)) * table ) (*| RClos of (string* (string* (opcode list)* table)) *)
and table = (string * answer) list
and opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
            | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT | CMP
           | PAREN | COND of (opcode list * opcode list) | TUPLE of int | PROJ of int*int | LET  | FABS of string * (opcode list) | FCALL | RETURN | BIND of (string * exptype)
           | SIMPLEDEF of string * (opcode list) | SEQCOMPOSE | PARCOMPOSE | LOCALDEF| RABS of string * (string * (opcode list))

let rec compile ex = match ex with
    Bool(b)-> [BCONST(b)]
  | Integer(n)-> [NCONST(mk_big n)]
  | V(x) -> [VAR(x)]
  | Abs(e) -> (compile(e))@[ABS]
  | Negative(e) -> (compile(e)) @[UNARYMINUS]
  | Not(e) -> (compile(e))@[NOT]
  |Plus(e1,e2) -> (compile(e2))@(compile(e1))@[PLUS]
  |Sub(e1,e2) ->(compile(e2))@(compile(e1))@[MINUS]
  |Mult(e1,e2)->(compile(e2))@(compile(e1))@[MULT]
  | Div(e1,e2) ->(compile(e2))@(compile(e1))@[DIV]
  |Rem(e1,e2) ->(compile(e2))@(compile(e1))@[REM]
  | And(e1,e2) ->(compile(e2))@(compile(e1))@[CONJ]
  | Or(e1,e2)->(compile(e2))@(compile(e1))@[DISJ]
  | Cmp(e1) -> (compile(e1))@[CMP]
  |Equals(e1,e2) -> (compile e2)@(compile e1)@[EQS]
  |GreaterTE(e1,e2)->(compile e2)@(compile e1)@[GTE]
  |LessTE(e1,e2)-> (compile e2)@(compile e1)@[LTE]
  |GreaterT(e1,e2)->(compile e2)@(compile e1)@[GT]
  |LessT(e1,e2)->(compile e2)@(compile e1)@[LT]
  |InParen(e)-> (compile e)@[PAREN]
  |If_Then_Else(e0,e1,e2)-> (compile e0)@[COND(compile e1,compile e2)]
  |Project((i,n),e)-> compile(e)@[PROJ(i,n)]
  |Tuple(n,elist)->let fn a b = (compile a) @ b in
    (List.fold_right fn (List.rev elist) [])@[TUPLE(n)]
  |Lambda((V(x),t),e)-> [FABS(x,(compile(e))@[RETURN])]
  | RLambda( (fname,retType),(V(x),t),e)->[RABS(fname,(x,compile(e)@[RETURN])  )]
                          (*|RecursiveAbs(fname,t,e) -> [RABS(fname,compile(e))]*)
  |App(e1,e2)-> (compile e1)@ (compile e2)@[FCALL]
  | Let(Simple((x,t),e1),e)-> (compile e1)@ [BIND(x,t)] @ (compile e) @ [LET]
  | Lambda(_,e)-> raise Invalid_Parameter
  | RLambda(_,_,e)-> raise Invalid_Parameter
  | Let(_,e)-> raise Invalid_Parameter

let rec getFirstn mlist n =
  if n = 0 then []
  else if n < 0 then raise Invalid_Parameter
  else match mlist with
      [] -> raise Invalid_Parameter
    | m::m1 -> m::(getFirstn m1 (n-1))

let rec removeFirstn mlist n =
  if n = 0 then mlist
  else if n<0 then raise Invalid_Parameter
  else match mlist with
      [] -> raise Invalid_Parameter
    | m::m1 -> removeFirstn (m1) (n-1)

let rec getFromTable x e = match e with
    []-> raise VariableNotFound
  | e1::e2 -> if x = fst e1 then snd e1
    else getFromTable x e2


let rec execute stack env opc dump =
  match (stack,env,opc,dump) with
    ( cl::s, e, [], (s1,e1,c1)::d) -> execute (cl::s1) e1 c1 d (*http://www.cse.iitd.ernet.in/~sak/courses/pl/opsem.pdf rules used from here*)
  | (cl::s,e,[],[])-> cl
  | ([],e,[],(s1,e1,c1)::d)-> execute s1 e1 c1 d
  | (s,e,VAR(x)::c,d)->
        execute ((getFromTable x e)::s) e c d
  | (s,e,(FABS(x,c))::c1,d)->  execute ((VClos((x,c),e))::s) e c1 d
  | (s,e, (RABS(fname,(x,c)))::c1, d ) -> execute (RClos(fname,(x,c),e)::s) ((fname,RClos(fname,(x,c),e))::e)  c1 d
  | (a::s,e, RETURN::c, (s1,e1,c1)::d)-> execute (a::s1) e1 c1 d
  | (a::VClos((x,c),e)::s,e1,FCALL::c1,d) -> execute [] ((x,a)::e) c ((s,e1,c1)::d)
  | (a::RClos(fname,(x,c),e)::s, e1, FCALL::c1,d) -> execute [] ((x,a)::(fname,RClos(fname,(x,c),e))::e) c ((s,e1,c1)::d)

  | (s,e, BCONST(b)::c,d) -> execute (B(b)::s)  e c d
  | (s,e, NCONST(n)::c, d)-> execute (Num(n)::s) e c d
  | ( Num(n)::s,e,ABS::c,d)-> execute (Num(abs n)::s) e c d
  | (Num(n)::s,e, UNARYMINUS::c,d) -> execute (Num(minus n)::s) e c d (** []  *)
  | (B(b)::s,e, NOT::c, d)-> execute (B(not b)::s) e c d
  | ( Num(n1)::Num(n2)::s,e, PLUS::c,d) -> execute ((Num(add n1 n2))::s) e c d
  | ( Num(n1)::Num(n2)::s,e, MINUS::c,d) -> execute ((Num(sub n1 n2))::s) e c d
  | ( Num(n1)::Num(n2)::s,e, MULT::c,d) -> execute ((Num(mult n1 n2))::s) e c d
  | ( Num(n1)::Num(n2)::s,e, DIV::c,d) -> execute ((Num(div n1 n2))::s) e c d
  | ( Num(n1)::Num(n2)::s,e, REM::c,d) -> execute ((Num(rem n1 n2))::s) e c d
  | ( B(b1)::B(b2)::s,e, CONJ::c,d) -> execute ((B(b1&&b2))::s) e c d
  |  ( B(b1)::B(b2)::s,e, DISJ::c,d) -> execute ((B(b1||b2))::s) e c d
  | ( Num(n1)::Num(n2)::s,e, EQS::c,d) -> execute ((B(eq n1 n2))::s) e c d
  | ( Num(n1)::Num(n2)::s,e, GTE::c,d) -> execute ((B(geq n1 n2))::s) e c d
  | ( Num(n1)::Num(n2)::s,e, LTE::c,d) -> execute ((B(leq n1 n2))::s) e c d
  | ( Num(n1)::Num(n2)::s,e, GT::c,d) -> execute ((B(gt n1 n2))::s) e c d
  | ( Num(n1)::Num(n2)::s,e, LT::c,d) -> execute ((B(lt n1 n2))::s) e c d
  | ( Num(n1)::s,e, CMP::c,d) -> execute ((B(gt n1 (mk_big(0)) ))::s) e c d
  |  (s,e, PAREN::c,d) -> execute s e c d
  |  (B(b)::s, e, COND(c1,c2)::c, d)-> execute s e (if b then c1@c else c2@c) d
  |  (s,e,TUPLE(n)::c,d) ->  execute  ((Tup(n, getFirstn s n))::(removeFirstn s n)) e c d
  |  (Tup(_,alist)::s,e,PROJ(i,n)::c,d)-> execute ((List.nth alist (i-1) )::s) e c d
  |  (a::s,e, BIND(x,t)::c, d) ->  execute s ((x,a)::e)  c  d
  |  (s,e1::e, LET::c,d) -> execute s e (c) d


  | _ -> raise MachineStuck
let getAnswer ex tab = execute [] tab (compile ex) []
