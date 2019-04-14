open A0
exception InvalidStackException
exception Invalid_Parameter
exception Not_implemented
type expr =
    V of string
  | Bool of bool
  | Integer of int
  (**)
  | Lambda of (expr * expr)
  | App of (expr * expr)
  (**)
  | Plus of (expr * expr)
  | Mult of (expr * expr)
  | Minus of (expr* expr)
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
  | Project (int*int) * (expr)
  | Let of definition * expr

and definition =
    Simple of (string* exptype)* expr
type  exptype = Tint | Tbool | Tfunc of (exptype * exptype) | Ttuple(exptype list)


type closure = Clos of (expr * table) | TClos (closure list)
and table = ((string)*( Clos of (expr * table)) ) list

type value  = NumVal of int | BoolVal of bool | TupVal of int * (value list) | ValClos((string* expr) * table)

type answer = Bool of bool | Num of bigint | Tup of int * (answer list) | VClos((string* expr)*table)

type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
            | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT | CMP
            | PAREN | COND | TUPLE of int | PROJ of int*int | LET | FABS of string * (opcode list) | FCALL | RETURN
            | SIMPLEDEF of string * (opcode list) | SEQCOMPOSE | PARCOMPOSE | LOCALDEF

type stackElement = ANS of answer | CompileClos(string*(opcode list)* table)


let rec compile ex = match ex with
    Bool(b)-> [BCONST(b)]
  | Integer(n)-> [NCONST(mk_big n)]
  | V(x) -> [VAR(x)]
  | Abs(e) -> (compile(e))@[ABS]
  | Negative(e) -> (compile(e)) @[UNARYMINUS]
  | Not(e) -> (compile(e))@[NOT]
  |Add(e1,e2) -> (compile(e2))@(compile(e1))@[PLUS]
  |Sub(e1,e2) ->(compile(e2))@(compile(e1))@[MINUS]
  |Mult(e1,e2)->(compile(e2))@(compile(e1))@[MULT]
  |Div(e1,e2) ->(compile(e2))@(compile(e1))@[DIV]
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
  |If_Then_Else(e0,e1,e2)-> (compile e0)@[COND(compile e1,e2)]
  |Project((i,n),e)-> compile(e)@[PROJ(i,n)]
  |Tuple(n,elist)->let fn a b = (compile a) @ b in
    (List.fold_right fn (List.rev elist) [])@[TUPLE(n)]
  |Lambda(V(x),e)-> [FABS(x,(compile(e))@[RETURN])]
  |App(e1,e2)-> (compile e1)@ (compile e2)@[FCALL]


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

let rec runSECD stack gamma oplist dump = match oplist with
    (FABS(x,c))::c1 ->   runSECD (CompileClos(x,c,gamma))::stack gamma c1 dump
  | FCALL::c -> begin
      match stack with
        (ANS(a))::(CompileClos(x,c1,gamma1))::s3 -> runSECD [] (x,a)::gamma1  c1  (stack,gamma,c)::dump
      | _ -> raise InvalidStackException
    end

  | RETURN::c3 -> begin
    match (stack,dump) with
        (a::stack1 , (stack2,gamma1,c2)::dump1) ->
        runSECD a::stack2 gamma1 c2 dump1
      | _ -> raise InvalidStackException
    end
  | (COND(c1,c2))::c3 ->
    begin
      match stack with
        (ANS(Bool(b)))::stack1 -> runSECD  stack1 gamma (if b then c1 else c2)@c3
      | _ -> raise InvalidStackException
    end
  | (TUPLE(n))::c ->
      let extractAns ans = match ans with
          ANS(a)-> a
         | _ ->raise Invalid_Parameter
      in
      runSECD  (ANS(Tup(n,List.map extractAns(List.rev getFirstn stack n))))::(removeFirstn stack n)  gamma c dump
  | (PROJ(i,n))::c ->begin
    match stack with
      (ANS(Tup(n,alist)))::stack1 -> runSECD   (ANS(List.nth alist (i-1)))::stack1 gamma c dump
    | _ -> raise InvalidStackException
  end
  | PAREN::c -> runSECD stack gamma c dump
  | LT::c -> begin match stack with
      (ANS(Num(n1)))::(ANS(Num(n2)))::s -> runSECD (ANS(Bool(lt n1 n2)))::s gamma c dump
    | _ -> raise InvalidStackException
    end
  | GT::c ->begin match stack with
        (ANS(Num(n1)))::(ANS(Num(n2)))::s -> runSECD (ANS(Bool(gt n1 n2)))::s gamma c dump
      | _ -> raise InvalidStackException
    end
  | LTE::c ->
    begin match stack with
        (ANS(Num(n1)))::(ANS(Num(n2)))::s -> runSECD (ANS(Bool(leq n1 n2)))::s gamma c dump
      | _ -> raise InvalidStackException
    end
  | GTE::c ->
    begin match stack with
        (ANS(Num(n1)))::(ANS(Num(n2)))::s -> runSECD (ANS(Bool(geq n1 n2)))::s gamma c dump
      | _ -> raise InvalidStackException
    end
  | CMP::c -> match stack with
      (ANS(Num(n)))::s -> runSECD (ANS(Bool(gt n (mk_big(0)))))::s gamma c dump

  | DISJ::c ->
      begin match stack with
          (ANS(Bool(b1)))::(ANS(Bool(b2)))::s -> runSECD (ANS(Bool( b1|| b2)))::s gamma c dump
        | _ -> raise InvalidStackException
      end
  | CONJ::c ->

    begin match stack with
        (ANS(Bool(b1)))::(ANS(Bool(b2)))::s -> runSECD (ANS(Bool( b1&& b2)))::s gamma c dump
      | _ -> raise InvalidStackException
    end
  | REM::c ->
  begin match stack with
      (ANS(Num(n1)))::(ANS(Num(n2)))::s -> runSECD (ANS(Num(rem n1 n2)))::s gamma c dump
    | _ -> raise InvalidStackException
  end
  | DIV::c ->

    begin match stack with
        (ANS(Num(n1)))::(ANS(Num(n2)))::s -> runSECD (ANS(Num(div n1 n2)))::s gamma c dump
      | _ -> raise InvalidStackException
    end
  | MULT::c ->
    begin match stack with
        (ANS(Num(n1)))::(ANS(Num(n2)))::s -> runSECD (ANS(Num(mult n1 n2)))::s gamma c dump
      | _ -> raise InvalidStackException
    end
  | MINUS::c ->

    begin match stack with
        (ANS(Num(n1)))::(ANS(Num(n2)))::s -> runSECD (ANS(Num(sub n1 n2)))::s gamma c dump
      | _ -> raise InvalidStackException
    end
  | PLUS::c ->

    begin match stack with
        (ANS(Num(n1)))::(ANS(Num(n2)))::s -> runSECD (ANS(Num(add n1 n2)))::s gamma c dump
      | _ -> raise InvalidStackException
    end
  | NOT::c -> begin
      match stack with
        (ANS(Bool(b)))::s -> runSECD (ANS(Bool(not b)))::s gamma c dump
      | _ -> raise InvalidStackException
    end
  | ABS::c ->
    begin
      match stack with
        (ANS(Num(n)))::s -> runSECD (ANS(Num(abs n)))::s gamma c dump
      | _ -> raise InvalidStackException

    end
  | UNARYMINUS::c ->
    begin
      match stack with
        (ANS(Num(n)))::s -> runSECD (ANS(Num(minus n)))::s gamma c dump
      | _ -> raise InvalidStackException

    end
  | VAR(x)::c -> raise Not_implemented
  | BCONST(b)::c-> runSECD (ANS(Bool(b)))::stack gamma c dump
  | NCONST(n)::c-> runSECD (ANS(Num(n)))::stack gamma c dump
  | [] ->raise Not_implemented
