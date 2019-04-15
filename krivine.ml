
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
  | Lambda of (expr * expr)
  | App of (expr * expr)
      (**)
  | Plus of (expr * expr)
  | Mult of (expr * expr)
  | Minus of (expr* expr)
        (**)
  | And of (expr * expr)
  | Or of (expr * expr)
        (**)
  | Not of (expr)
  | Negative of (expr)
  | Abs of (expr)
        (**)
  | Cmp of expr(*GT 0*)
  (**)
  (**)
  | If_Then_Else of (expr * expr * expr)
        (**)
  | Tuple of int*(expr list)
               (**)
  | Project of (int*int) * expr

type  exptype = Tint | Tbool | Tfunc of (exptype * exptype) | Ttuple of (exptype list)


type closure = Clos of (expr * table) | Tclos of (closure list)
and table = (string * closure )list

type answer  = NumVal of int | BoolVal of bool | TupVal of int * (answer list) | VClos of ((string* expr) * table)

type stackElement = CLOSURE of closure | ADD of closure | MULT of closure | AND of closure | OR of closure | NOT | NEGATIVE | ABS| CMP | IFTE of (closure * closure) | TUP of ( closure list * closure list)| PROJ of (int*int)

let makeClosure  gamma e = Clos(e,gamma)

let absInt n = if n>=0 then n else -n

let rec getVarFromTable x gamma = match gamma with
    [] -> raise ElementNotFoundException
  | g1::g2 -> if (fst g1) = x then snd g1 else getVarFromTable x g2

let getExp clos = match clos with
    Clos(exp, gamma) -> gamma
  | _ -> raise Invalid_Parameter

let rec runKrivine clos stack =  match clos with
   Clos(V(x),gamma) ->
      runKrivine (getVarFromTable x gamma) stack
  | Clos(App(e1,e2),gamma) ->
    runKrivine (Clos(e1,gamma))  ((CLOSURE((Clos(e2,gamma))))::stack)

  | Clos (Lambda(V(x),e),gamma) -> begin
      match stack with
        [] -> clos
      | CLOSURE(clos1)::stack1 -> runKrivine (Clos(e, (x,clos1)::gamma))  stack1

      | (TUP( r_completed, current::(rem1::rem2)))::stack1 ->(*current is the closure which when run on the krivine machine gave clos*)
        runKrivine  rem1  ((TUP( clos::r_completed, rem2 ))::stack1)

      | TUP(r_completed, current::[])::stack1 ->
        (*  runKrivine Clos(Tuple(1+List.length r_completed , List.map getExp (List.rev (clos::r_completed))))  stack1*)
        Tclos( List.rev (clos::r_completed))

      | _ -> raise InvalidStackException
    end

  | Clos(Integer(n),gamma) ->begin
    match stack with
        ADD(Clos(Integer(m),gamma1))::stack1 ->
        runKrivine (Clos(Integer(m+n),gamma)) stack1
      | ADD(clos1)::stack1 ->
        runKrivine clos1  ((ADD(clos))::stack1)
      | MULT(Clos(Integer(m),gamma1))::stack1 ->
        runKrivine (Clos(Integer(m*n),gamma)) stack1
      | MULT(clos1)::stack1 ->
        runKrivine clos1  ((ADD(clos))::stack1)
      | NEGATIVE :: stack1 ->
        runKrivine (Clos(Integer(-n),gamma)) stack1
      | ABS:: stack1 ->
        runKrivine (Clos(Integer(absInt n),gamma)) stack1
      | CMP:: stack1 ->
        runKrivine (Clos(Bool(n>0),gamma)) stack1

      | [] -> clos
      | TUP( r_completed, current::(rem1::rem2))::stack1 ->(*current is the closure which when run on the krivine machine gave clos*)
        runKrivine  rem1  ((TUP( clos::r_completed, rem2 ))::stack1)

      | TUP(r_completed, current::[])::stack1 ->
         Tclos( List.rev (clos::r_completed))

      | _ -> raise InvalidStackException

    end
  | Clos(Bool(n),gamma) ->begin match stack with
       AND(Clos(Bool(m),gamma1))::stack1 ->
       runKrivine (Clos(Bool(m&&n),gamma)) stack1
    | AND(clos1)::stack1 ->
      runKrivine clos1  ((AND(clos))::stack1)
    | OR(Clos(Bool(m),gamma1))::stack1 ->
      runKrivine (Clos(Bool(m||n),gamma)) stack1
    | OR(clos1)::stack1 ->
      runKrivine clos1  ((OR(clos))::stack1)
    | NOT :: stack1 ->
      runKrivine (Clos(Bool(not n),gamma))  stack1
    | ( IFTE(clos1,clos2))::stack1->
      runKrivine (if n then clos1 else clos2) stack1
    | [] -> clos
    | TUP( r_completed, current::(rem1::rem2))::stack1 ->(*current is the closure which when run on the krivine machine gave clos*)
      runKrivine  rem1  (TUP( clos::r_completed, rem2 )::stack1)

    | TUP(r_completed, current::[])::stack1 ->
      Tclos( List.rev (clos::r_completed))
    | ((IFTE(clos1,clos2))::stack1) ->
        if n = true then runKrivine clos1 stack1
        else runKrivine clos2 stack1
    | _ -> raise InvalidStackException
    end

  | Clos(Plus(e1,e2),gamma) ->
    runKrivine (Clos(e1,gamma)) ((ADD(Clos(e2,gamma)))::stack)
  | Clos(Minus(e1,e2),gamma)->
    runKrivine (Clos(e1, gamma))  ((ADD(Clos(Negative(e2),gamma)))::stack)
  | Clos(Mult(e1,e2),gamma)->
    runKrivine (Clos(e1,gamma))  ((MULT(Clos(e2,gamma)))::stack)
  | Clos(Negative(e),gamma) ->
    runKrivine (Clos(e, gamma)) (NEGATIVE::stack)
  | Clos(Abs(e),gamma) ->
    runKrivine (Clos(e,gamma)) (ABS::stack)
  | Clos(And(e1,e2),gamma)->
    runKrivine (Clos(e1,gamma)) ((AND(Clos(e2,gamma)))::stack)
  | Clos(Or(e1,e2),gamma)->
    runKrivine (Clos(e1,gamma)) ((OR(Clos(e2,gamma)))::stack)
  | Clos(Not(e),gamma) ->
    runKrivine (Clos(e,gamma)) (NOT::stack)
  | Clos(Cmp(e),gamma) ->
    runKrivine (Clos(e,gamma)) (CMP::stack)

  | Clos(If_Then_Else(e0,e1,e2),gamma)->
    runKrivine (Clos(e0,gamma)) ((IFTE(Clos(e1,gamma),Clos(e2,gamma)))::stack)
  | Clos(Tuple(n,elist),gamma)->
    runKrivine (Clos(List.hd elist,gamma))   ((TUP( [] ,List.map (makeClosure gamma) elist))::stack) (*TUP(l1,l2) the head of l2 is the main closure that is being evaluated now and l1 is list of evaluated closures in reverse order*)

  | Clos(Project((i,n),tuple),gamma)->
    runKrivine (Clos(tuple,gamma))  ((PROJ(i,n))::stack)
  | Tclos(clist)->begin
      match stack with
        PROJ(i,n)::stack1 ->
        runKrivine  (List.nth clist (i-1)) stack1
      | [] -> clos
      | TUP( r_completed, current::(rem1::rem2))::stack1 ->(*current is the closure which when run on the krivine machine gave clos*)
        runKrivine  rem1  ((TUP( clos::r_completed, rem2 ))::stack1)

      | TUP(r_completed, current::[])::stack1 ->
        Tclos( List.rev (clos::r_completed))
      | _ -> raise InvalidStackException
     end
  | _ -> raise InvalidClosureException
let rec resolveClosure clos = match clos with
    Clos(Integer(n),gamma)-> NumVal(n)
  | Clos(Bool(b),gamma) -> BoolVal(b)
  | Clos(Lambda(V(x),e),gamma) -> VClos((x,e),gamma)
  | Tclos(clist)->
    TupVal(List.length clist, List.map resolveClosure clist)
  | _ -> raise UnResolvableClosure

let getAnswer e gamma = resolveClosure (runKrivine (Clos(e,gamma)) [])
