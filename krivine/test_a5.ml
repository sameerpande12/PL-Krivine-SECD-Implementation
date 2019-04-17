#load "a0.cmo";;
#load "krivine.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
#load "a4.cmo";;

open A0;;
open Krivine;;
open A2;;
open A3;;
open A4;;

exception Not_implemented
    (*  (* Helper function to print *)
let rec print_tree tr = match tr with
  N a -> "INT " ^ (string_of_int a)
  | _ -> raise Not_implemented
;;
let rec print_answer tr = match tr with
  Num a -> print_num a
  | Bool a -> string_of_bool a
  | _ -> raise Not_implemented
;;
let rec print_value tr = match tr with
  NumVal a -> string_of_int a
  | BoolVal a -> string_of_bool a
  | _ -> raise Not_implemented
;;
let rec print_def df = match df with
    Simple((l,ltype),r) -> "def " ^ l ^ " = " ^ (print_tree r)
  | _ -> raise Not_implemented
;;
*)

(* Input is given as value and output is an answer *)
(*let rec toAnswer v = match v with
  NumVal a     -> Num (mk_big a)
| BoolVal b    -> Bool b
| TupVal (n,xs) -> Tup (n, List.map toAnswer xs);;
*)
(* Input is given as string and output is an answer
   let binding rho s = toAnswer (rho s);;
*)


(* Both use the same lexer in A1 but different parser in A3 *)
let exp_parser s rho = A3.exp_parser A2.read (Lexing.from_string s) ;;
let def_parser s rho = A3.def_parser A2.read (Lexing.from_string s) ;;
let type_parser s rho = A3.type_parser A2.read (Lexing.from_string s) ;;
let scan s = A2.read (Lexing.from_string s);;
(* Input is given as string and output is a value *)
let rho s = match s with
    "X" -> NumVal ((5))
  |  "Y" -> BoolVal (true)

  | _ -> raise Not_implemented
;;

let env = [("X",NumVal(5));("Y",BoolVal(true))];;

(* Sample test case *)
let e = (exp_parser "\\X:Tint.Y" rho);;
let t = Tfunc (Tint, Tbool);;


let e = exp_parser "\\X:Tint.X" rho;;
let t = Tfunc(Tint, Tint);;
let e = exp_parser "let def X:Tbool = Y in X end" rho;;
(* Type assumptions as a list of tuples of the form (variable name, type) *)
(*let g = [ ("X", Tint); ("Y", Tbool); ("Z", Ttuple [Tint ; Tbool ; Tint]); ("W", Tfunc (Tint, Tbool))];;
let d = (def_parser "def U:Tint = X ; def V:Tbool = Y" rho);;
let g_dash = [("U", Tint); ("V", Tbool)];;

let parse s = exp_parser s rho;;
let srd myv = A2.read (Lexing.from_string myv);;
*)
let ten = exp_parser "10" rho;;
let seven = exp_parser "7" rho;;
let solve x = getAnswer (exp_parser x rho) [];;

let get e = getAnswer e [];;
let s6 = "\\X:(Tint).(let def A:Tint = X in let def B:Tint = X in A + B end end)"
let e6 = exp_parser s6 rho;;
get (App(e6,ten));;



let s1= " rec(Fib:Tint)->X:Tint.(if (X=0) \\/ (X = 1) then X else Fib(X-1) + Fib(X-2) fi ) "
let fib= exp_parser s1 rho

let s2 = " rec(Gcd:Tint)->Y:(Tint*Tint).(let def A:Tint = proj(1,2)Y in let def B:Tint = proj(2,2)Y in if (B = 0)then A else Gcd((B, A mod B)) fi end end )"
let gcd = exp_parser s2 rho






let s3 = "(4,0)";;
let e3 = exp_parser s3 rho;;
get (App(gcd,e3));;



(*
let s4 = " ( \\X:(Tint*Tint).
                    (let def A:Tint = proj(1,2)X in A end )
           )((3,4))";;
let e4 = exp_parser s4 rho;;

get e4;;
solve s4;;
*)
let s5 = "     (\\X:(Tint*Tint).( let def A:Tint = proj(1,2) X in A end )   )((1,2))";;
let e5 = exp_parser s5 rho;;


let s6 = "let def X:Tint = 1  in
                     let def X:Tint = X+1 in
                           let def X:Tint = X + 1 in
                                   let def Z:Tint = X + 1 in
                                           Z
                                   end  +
                              X
                            end +
                        X
                     end
                +X
 end"
let e6 = exp_parser s6 rho;;
get e6;;
