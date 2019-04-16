
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
  | RLambda of (string * (expr* exptype) * expr) (* fname, (variable of x,type), functionbody*)
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


let makeClosure  gamma e = Clos(e,gamma)

let absInt n = if n>=0 then n else -n

(*let printKeys (gamma:table) = match gamma with
    [] -> (*Printf.printf " "*)
  | g1::g2 -> (*Printf.printf "%s " (fst g1)*)
*)
let rec getVarFromTable (x:string) gamma =
  (*Printf.printf "Searching %s " x ;*)
  match gamma with
    [] -> raise ElementNotFoundException
  | g1::g2 -> (*Printf.printf "Iter %s "*) (fst g1);if (fst g1) = x then snd g1 else getVarFromTable x g2

let getExp clos = match clos with
    Clos(exp, gamma) -> gamma
  | _ -> raise Invalid_Parameter

let rec runKrivine clos stack =  match clos with
    Clos (InParen(e),gamma)->
    (*  printKeys gamma;*)
    (*Printf.printf "1\n";*)
    runKrivine (Clos(e,gamma)) stack
  |Clos(V(x),gamma) ->
    (*printKeys gamma;*)
    (*Printf.printf "2";*)
      runKrivine (getVarFromTable x gamma) stack
  | Clos(App(e1,e2),gamma) ->
    (*Printf.printf "3\n";*)
    runKrivine (Clos(e1,gamma))  ((CLOSURE((Clos(e2,gamma))))::stack)

  | Clos(Let( Simple((x,t),e1), e ),gamma)->
    (*printKeys gamma;*)
    (*Printf.printf "4\n";*)
    runKrivine (Clos(e1,gamma))  (LET(x,e,gamma)::UNBIND::stack)

  | Clos (RLambda(fname ,(V(x),t),e),gamma)->
    begin
      match stack with
        [] -> (*Printf.printf "5\n";*) Clos(RLambda(fname ,(V(x),t),e),(fname,clos)::gamma)
      | CLOSURE(clos1)::stack1 -> (*Printf.printf "6\n";*)runKrivine (Clos(e, (fname,clos)::(x,clos1)::gamma))  stack1

      | (TUP( r_completed, current::(rem1::rem2)))::stack1 ->(*current is the closure which when run on the krivine machine gave clos*)
      (*Printf.printf "7\n";*)  runKrivine  rem1  ((TUP( clos::r_completed, rem1::rem2 ))::stack1)

      | TUP(r_completed, current::[])::stack1 ->
        (*  runKrivine Clos(Tuple(1+List.length r_completed , List.map getExp (List.rev (clos::r_completed))))  stack1*)
(*Printf.printf "8\n";*)
        runKrivine (Tclos( List.rev (clos::r_completed))) stack1
      | LET(x,e,gamma1)::stack1->
(*Printf.printf "9\n";*)
        runKrivine (Clos(e,(x,clos)::gamma1))  stack1
      | UNBIND::stack1 ->
(*Printf.printf "10\n";*)
        runKrivine clos stack1

      | _ -> raise InvalidStackException
    end

  | Clos (Lambda((V(x),t),e),gamma) -> begin
      match stack with
        [] -> (*Printf.printf "11\n";*)clos
      | CLOSURE(clos1)::stack1 ->
        (*  printKeys ((x,clos1)::gamma);*)
        (*Printf.printf "12\n";*)
        runKrivine (Clos(e, (x,clos1)::gamma))  stack1

      | (TUP( r_completed, current::(rem1::rem2)))::stack1 ->(*current is the closure which when run on the krivine machine gave clos*)
        (*Printf.printf "13\n";*)runKrivine  rem1  ((TUP( clos::r_completed, rem1::rem2 ))::stack1)

      | TUP(r_completed, current::[])::stack1 ->
        (*Printf.printf "14\n";*)(*  runKrivine Clos(Tuple(1+List.length r_completed , List.map getExp (List.rev (clos::r_completed))))  stack1*)
       runKrivine (Tclos( List.rev (clos::r_completed))) stack1
      | LET(x,e,gamma1)::stack1->(*Printf.printf "15\n";*)
        runKrivine (Clos(e,(x,clos)::gamma1))  stack1
      | UNBIND::stack1 ->
        (*Printf.printf "16\n";*)
        runKrivine clos stack1

      | _ -> raise InvalidStackException
    end

  | Clos(Integer(n),gamma) ->begin
    match stack with
        ADD(Clos(Integer(m),gamma1))::stack1 ->(*Printf.printf "17\n";*)
        runKrivine (Clos(Integer(m+n),gamma)) stack1
      | ADD(clos1)::stack1 ->(*Printf.printf "18\n";*)
        runKrivine clos1  ((ADD(clos))::stack1)
      | MULT(Clos(Integer(m),gamma1))::stack1 ->(*Printf.printf "19\n";*)
        runKrivine (Clos(Integer(m*n),gamma)) stack1
      | MULT(clos1)::stack1 ->(*Printf.printf "20\n";*)
        runKrivine clos1  ((MULT(clos))::stack1)

      | DIV(NULL,Clos(Integer(m),gamma1))::stack1->(*Printf.printf "21\n";*)
        runKrivine (Clos(Integer(n/m),gamma)) stack1
      | DIV(NULL,clos1)::stack1->(*Printf.printf "22\n";*)
        runKrivine clos1  ((DIV(clos,NULL))::stack1)
      | DIV(Clos(Integer(m),gamma1),NULL)::stack1->(*Printf.printf "23\n";*)
        runKrivine (Clos(Integer(m/n),gamma)) stack1
      | DIV(clos1,NULL)::stack1->(*Printf.printf "24\n";*)
        runKrivine clos1  ((DIV(NULL,clos))::stack1)

      |REM(NULL,Clos(Integer(m),gamma1))::stack1->(*Printf.printf "25\n";*)
          runKrivine (Clos(Integer(n mod m),gamma)) stack1
      | REM(NULL,clos1)::stack1->(*Printf.printf "26\n";*)
        runKrivine clos1  ((REM(clos,NULL))::stack1)
      | REM(Clos(Integer(m),gamma1),NULL)::stack1->(*Printf.printf "27\n";*)
        runKrivine (Clos(Integer(m mod n),gamma)) stack1
      | REM(clos1,NULL)::stack1->(*Printf.printf "28\n";*)
        runKrivine clos1  ((REM(NULL,clos))::stack1)

      | EQS(NULL,Clos(Integer(m),gamma1))::stack1->(*Printf.printf "29\n";*)
        runKrivine (Clos(Bool(n = m),gamma)) stack1
      | EQS(NULL,clos1)::stack1->(*Printf.printf "30\n";*)
        runKrivine clos1  ((EQS(clos,NULL))::stack1)
      | EQS(Clos(Integer(m),gamma1),NULL)::stack1->(*Printf.printf "31\n";*)
        runKrivine (Clos(Bool(m = n),gamma)) stack1
      | EQS(clos1,NULL)::stack1->(*Printf.printf "32\n";*)
        runKrivine clos1  ((EQS(NULL,clos))::stack1)

      | LT(NULL,Clos(Integer(m),gamma1))::stack1->(*Printf.printf "33\n";*)
        runKrivine (Clos(Bool(n < m),gamma)) stack1
      | LT(NULL,clos1)::stack1->(*Printf.printf "34\n";*)
        runKrivine clos1  ((LT(clos,NULL))::stack1)
      | LT(Clos(Integer(m),gamma1),NULL)::stack1->(*Printf.printf "35\n";*)
        runKrivine (Clos(Bool(m < n),gamma)) stack1
      | LT(clos1,NULL)::stack1->(*Printf.printf "36\n";*)
        runKrivine clos1  ((LT(NULL,clos))::stack1)

      | LTE(NULL,Clos(Integer(m),gamma1))::stack1->(*Printf.printf "37\n";*)
        runKrivine (Clos(Bool(n <= m),gamma)) stack1
      | LTE(NULL,clos1)::stack1->(*Printf.printf "38\n";*)
        runKrivine clos1  ((LTE(clos,NULL))::stack1)
      | LTE(Clos(Integer(m),gamma1),NULL)::stack1->(*Printf.printf "39\n";*)
        runKrivine (Clos(Bool(m <= n),gamma)) stack1
      | LTE(clos1,NULL)::stack1->(*Printf.printf "40\n";*)
        runKrivine clos1  ((LTE(NULL,clos))::stack1)

      | GTE(NULL,Clos(Integer(m),gamma1))::stack1->(*Printf.printf "41\n";*)
        runKrivine (Clos(Bool(n >= m),gamma)) stack1
      | GTE(NULL,clos1)::stack1->(*Printf.printf "42\n";*)
        runKrivine clos1  ((GTE(clos,NULL))::stack1)
      | GTE(Clos(Integer(m),gamma1),NULL)::stack1->(*Printf.printf "43\n";*)
        runKrivine (Clos(Bool(m >= n),gamma)) stack1
      | GTE(clos1,NULL)::stack1->(*Printf.printf "44\n";*)
        runKrivine clos1  ((GTE(NULL,clos))::stack1)

      | GT(NULL,Clos(Integer(m),gamma1))::stack1->(*Printf.printf "45\n";*)
        runKrivine (Clos(Bool(n > m),gamma)) stack1
      | GT(NULL,clos1)::stack1->(*Printf.printf "46\n";*)
        runKrivine clos1  ((GT(clos,NULL))::stack1)
      | GT(Clos(Integer(m),gamma1),NULL)::stack1->(*Printf.printf "47\n";*)
        runKrivine (Clos(Bool(m > n),gamma)) stack1
      | GT(clos1,NULL)::stack1->(*Printf.printf "48\n";*)
        runKrivine clos1  ((GT(NULL,clos))::stack1)

      | NEGATIVE :: stack1 ->(*Printf.printf "49\n";*)
        runKrivine (Clos(Integer(-n),gamma)) stack1
      | ABS:: stack1 ->(*Printf.printf "50\n";*)
        runKrivine (Clos(Integer(absInt n),gamma)) stack1
      | CMP:: stack1 ->(*Printf.printf "51\n";*)
        runKrivine (Clos(Bool(n>0),gamma)) stack1

      | [] -> (*Printf.printf "52\n";*)clos
      | TUP( r_completed, current::(rem1::rem2))::stack1 ->(*current is the closure which when run on the krivine machine gave clos*)
(*Printf.printf "53\n";*)
        runKrivine  rem1  ((TUP( clos::r_completed, rem1::rem2 ))::stack1)

      | TUP(r_completed, current::[])::stack1 ->
(*Printf.printf "54\n";*)
        runKrivine (Tclos( List.rev (clos::r_completed))) stack1
      | LET(x,e,gamma1)::stack1->
        (*Printf.printf "\n";*)
        (*  printKeys ((x,clos)::gamma1); *)
(*Printf.printf "55\n";*)
        runKrivine (Clos(e,(x,clos)::gamma1))  stack1
      | UNBIND::stack1 ->
(*Printf.printf "56\n";*)
        runKrivine clos stack1


      | _ -> raise InvalidStackException

    end
  | Clos(Bool(n),gamma) ->begin match stack with
       AND(Clos(Bool(m),gamma1))::stack1 ->(*Printf.printf "57\n";*)
       runKrivine (Clos(Bool(m&&n),gamma)) stack1
    | AND(clos1)::stack1 ->(*Printf.printf "58\n";*)
      runKrivine clos1  ((AND(clos))::stack1)
    | OR(Clos(Bool(m),gamma1))::stack1 ->(*Printf.printf "59\n";*)
      runKrivine (Clos(Bool(m||n),gamma)) stack1
    | OR(clos1)::stack1 ->(*Printf.printf "60\n";*)
      runKrivine clos1  ((OR(clos))::stack1)
    | NOT :: stack1 ->(*Printf.printf "61\n";*)
      runKrivine (Clos(Bool(not n),gamma))  stack1
    | ( IFTE(clos1,clos2))::stack1->(*Printf.printf "62\n";*)
      runKrivine (if n then clos1 else clos2) stack1
    | [] -> (*Printf.printf "63\n";*)clos
    | TUP( r_completed, current::(rem1::rem2))::stack1 ->(*current is the closure which when run on the krivine machine gave clos*)
    (*Printf.printf "64\n";*)  runKrivine  rem1  (TUP( clos::r_completed, rem1::rem2 )::stack1)

    | TUP(r_completed, current::[])::stack1 ->
      (*Printf.printf "65\n";*) runKrivine (Tclos( List.rev (clos::r_completed))) stack1
    | IFTE(clos1,clos2)::stack1 ->
(*Printf.printf "66\n";*)
      if n = true then runKrivine clos1 stack1
        else runKrivine clos2 stack1
    | LET(x,e,gamma1)::stack1->
(*Printf.printf "67\n";*)
      runKrivine (Clos(e,(x,clos)::gamma1))  stack1
    | UNBIND::stack1 ->
(*Printf.printf "68\n";*)
      runKrivine clos stack1

    | _ -> raise InvalidStackException
    end

  | Clos(Plus(e1,e2),gamma) ->
    (*Printf.printf "69\n";*)
    runKrivine (Clos(e1,gamma)) ((ADD(Clos(e2,gamma)))::stack)
  | Clos(Sub(e1,e2),gamma)->
    (*Printf.printf "70\n";*)
    runKrivine (Clos(e1, gamma))  ((ADD(Clos(Negative(e2),gamma)))::stack)
  | Clos(Mult(e1,e2),gamma)->(*Printf.printf "71\n";*)
    runKrivine (Clos(e1,gamma))  ((MULT(Clos(e2,gamma)))::stack)
  | Clos(Div(e1,e2),gamma)->(*Printf.printf "72\n";*)
    runKrivine (Clos(e1,gamma)) ((DIV(NULL,Clos(e2,gamma))) :: stack)
  | Clos(Rem(e1,e2),gamma)->(*Printf.printf "73\n";*)
    runKrivine (Clos(e1,gamma)) ((REM(NULL,Clos(e2,gamma))) :: stack)

  | Clos(Equals(e1,e2),gamma)->(*Printf.printf "74\n";*)
    runKrivine (Clos(e1,gamma)) ((EQS(NULL,Clos(e2,gamma))) :: stack)
  | Clos(GreaterTE(e1,e2),gamma)->(*Printf.printf "75\n";*)
    runKrivine (Clos(e1,gamma)) ((GTE(NULL,Clos(e2,gamma))) :: stack)
  | Clos(GreaterT(e1,e2),gamma)->(*Printf.printf "76\n";*)
    runKrivine (Clos(e1,gamma)) ((GT(NULL,Clos(e2,gamma))) :: stack)

  | Clos(LessTE(e1,e2),gamma)->(*Printf.printf "77\n";*)
    runKrivine (Clos(e1,gamma)) ((LTE(NULL,Clos(e2,gamma))) :: stack)
  | Clos(LessT(e1,e2),gamma)->(*Printf.printf "78\n";*)
    runKrivine (Clos(e1,gamma)) ((LT(NULL,Clos(e2,gamma))) :: stack)

  | Clos(Negative(e),gamma) ->(*Printf.printf "79\n";*)
    runKrivine (Clos(e, gamma)) (NEGATIVE::stack)
  | Clos(Abs(e),gamma) ->(*Printf.printf "80\n";*)
    runKrivine (Clos(e,gamma)) (ABS::stack)
  | Clos(And(e1,e2),gamma)->(*Printf.printf "81\n";*)
    runKrivine (Clos(e1,gamma)) ((AND(Clos(e2,gamma)))::stack)
  | Clos(Or(e1,e2),gamma)->(*Printf.printf "82\n";*)
    runKrivine (Clos(e1,gamma)) ((OR(Clos(e2,gamma)))::stack)
  | Clos(Not(e),gamma) ->(*Printf.printf "83\n";*)
    runKrivine (Clos(e,gamma)) (NOT::stack)
  | Clos(Cmp(e),gamma) ->(*Printf.printf "84\n";*)
    runKrivine (Clos(e,gamma)) (CMP::stack)

  | Clos(If_Then_Else(e0,e1,e2),gamma)->(*Printf.printf "85\n";*)
    runKrivine (Clos(e0,gamma)) ((IFTE(Clos(e1,gamma),Clos(e2,gamma)))::stack)
  | Clos(Tuple(n,elist),gamma)->(*Printf.printf "86\n";*)

    runKrivine (Clos(List.hd elist,gamma))   ((TUP( [] ,List.map (makeClosure gamma) elist))::stack) (*TUP(l1,l2) the head of l2 is the main closure that is being evaluated now and l1 is list of evaluated closures in reverse order*)

  | Clos(Project((i,n),tuple),gamma)->(*Printf.printf "87\n";*)
    runKrivine (Clos(tuple,gamma))  ((PROJ(i,n))::stack)
  | Tclos(clist)->begin
      match stack with
        PROJ(i,n)::stack1 ->(*Printf.printf "88\n";*)
        runKrivine  (List.nth clist (i-1)) stack1
      | [] ->(*Printf.printf "89\n";*)
                  clos
      | TUP( r_completed, current::(rem1::rem2))::stack1 ->(*current is the closure which when run on the krivine machine gave clos*)
(*Printf.printf "90\n";*)
        runKrivine  rem1  ((TUP( clos::r_completed, rem1::rem2 ))::stack1)

      | TUP(r_completed, current::[])::stack1 ->
(*Printf.printf "91\n";*)
         runKrivine (Tclos( List.rev (clos::r_completed))) stack1
      | LET(x,e,gamma1)::stack1->
        (*Printf.printf "92\n";*)
        runKrivine (Clos(e,(x,clos)::gamma1))  stack1

      | UNBIND::stack1 ->
        (*Printf.printf "93\n";*)
        runKrivine clos stack1

      | _ -> raise InvalidStackException
     end
  | _ -> raise InvalidClosureException
let rec resolveClosure clos = match clos with
    Clos(Integer(n),gamma)-> NumVal(n)
  | Clos(Bool(b),gamma) -> BoolVal(b)
  | Clos(Lambda( (V(x),t),e),gamma) -> VClos((x,e),gamma)
  | Tclos(clist)->
    TupVal(List.length clist, List.map resolveClosure clist)
  | _ -> raise UnResolvableClosure

let getAnswer e gamma = resolveClosure (runKrivine (Clos(e,gamma)) [])
