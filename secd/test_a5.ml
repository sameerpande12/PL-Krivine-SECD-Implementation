#load "a0.cmo";;
#load "secd.cmo";;
#load "lexer.cmo";;
#load "parser.cmo";;
#load "typechecker.cmo";;

open A0;;
open Secd;;
open Lexer;;
open Parser;;
open Typechecker;;

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
    "X" -> Num (mk_big(5))
  |  "Y" -> B (true)

  | _ -> raise Not_implemented
;;

let env = [("X",Num(mk_big(5)));("Y",B(true))];;

(* Sample test case *)
let e = (exp_parser "\\X:Tint.Y" rho);;
let t = Tfunc (Tint, Tbool);;


let e = exp_parser "\\X:Tint.X" rho;;
let t = Tfunc(Tint, Tint);;
let e = exp_parser "let def X:Tbool = Y in X end" rho;;(* Type assumptions as a list of tuples of the form (variable name, type) *)
(*let g = [ ("X", Tint); ("Y", Tbool); ("Z", Ttuple [Tint ; Tbool ; Tint]); ("W", Tfunc (Tint, Tbool))];;
let d = (def_parser "def U:Tint = X ; def V:Tbool = Y" rho);;
let g_dash = [("U", Tint); ("V", Tbool)];;

let parse s = exp_parser s rho;;
let srd myv = A2.read (Lexing.from_string myv);;
*)
let e1= exp_parser " rec(Fib)->X:Tint.(if (X=0) \\/ (X = 1) then X else Fib(X-1) + Fib(X-2) fi ) " rho
let s = " rec(Gcd)->Y:(Tint*Tint).(let def A:Tint = proj(1,2)Y in let def B:Tint = proj(2,2)Y in if (B = 0)then A else Gcd((B, A mod B)) fi end end )"
let e2 = exp_parser s rho
