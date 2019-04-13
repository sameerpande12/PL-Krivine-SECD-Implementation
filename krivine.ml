open A0
exception Not_implemented
exception ElementNotFoundException
exception InvalidStackException
exception InvalidClosureException

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
        (**)
  | And of (expr * expr)
  | Or of (expr * expr)
        (**)
  | Not of (expr)
        (**)
  | Cmp of expr
  (**)
  (**)
  | If_Then_Else of (expr * expr * expr)


type  exptype = Tint | Tbool | Tfunc of (exptype * exptype)

type value  = Tint | Tbool

type closure = Clos of (expr * table)
and table = ((string)*( Clos of (expr * table)) ) list

type stackElement = CLOSURE of closure | ADD of closure | MULT of closure | AND of closre | OR of closure


let rec getVarFromTable x gamma = match gamma with
    [] -> raise ElementNotFoundException
  | g1::g2 -> if (fst g1) = x then snd g1 else getVarFromTable x g2

let rec runKrivine clos stack =  match clos with
    Clos(V(x),gamma) -> runKrivine (getVarFromTable x gamma) stack
  | Clos(App(e1,e2),gamma) ->
    runKrivine (Clos(e1,gamma))  CLOSURE((Clos(e2,gamma)))::stack
  | Clos(Lambda(V(x),e)) -> begin
      match stack with
        [] -> raise EmptyStackException
      | CLOSURE(clos1)::stack1 -> runKrivine Clos(e, (x,clos1)::gamma )  (List.tl stack)
      | _ -> raise InvalidStackException
    end

  | Clos(Integer(n),gamma) ->begin
    match stack with
      ADD::CLOSURE(Clos(Integer(m),gamma1))::stack1 ->
                    runKrivine Clos(Integer(m+n),gamma) stack1
      | ADD::CLOSURE(clos1)::stack1 ->
        runKrivine clos1   ADD::CLOSURE(clos)::stack1
    |  MULT::CLOSURE(Clos(Integer(m),gamma1))::stack1 ->
                    runKrivine Clos(Integer(m*n),gamma) stack1
    | MULT::CLOSURE(clos1)::stack1 ->
      runKrivine clos1   MULT::CLOSURE(clos)::stack1
    | [] -> clos
    | _ -> raise InvalidStackException

    end
  | Clos(Bool(n),gamma) ->begin
      AND::CLOSURE(Clos(Bool(m),gamma1))::stack1 ->
      runKrivine Clos(Bool(n&&m),gamma) stack1
    | AND::CLOSURE(clos1)::stack1 ->
      runKrivine clos1 AND::CLOSURE(clos)::stack1
    |  OR::CLOSURE(Clos(Bool(m),gamma1))::stack1 ->
      runKrivine Clos(Bool(n||m),gamma) stack1
    | OR::CLOSURE(clos1)::stack1 ->
      runKrivine clos1 AND::CLOSURE(clos)::stack1

    end
  | Clos(Plus(e1,e2),gamma) ->
    runKrivine Clos(gamma,e1) ADD::CLOSURE(clos)::stack
  | Clos(Mult(e1,e2),gamma)->
    runKrivine Clos(gamma,e1)  MULT::CLOSURE(clos)::stack
  | Clos(And(e1,e2),gamma)->
    runKrivine Clos(gamma,e1) AND::CLOSURE(clos)::stack
  | Clos(Or(e1,e2),gamma)->
    runKrivine Clos(gamma,e1) OR::CLOSURE(clos)::stack
