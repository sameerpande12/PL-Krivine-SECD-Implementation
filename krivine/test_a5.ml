#load "a0.cmo";;
#load "krivine.cmo";;
#load "lexer.cmo";;
#load "parser.cmo";;
#load "typechecker.cmo";;

open A0;;
open Krivine;;
open Lexer;;
open Parser;;
open Typechecker;;

exception Not_implemented



let exp_parser s = Parser.exp_parser Lexer.read (Lexing.from_string s) ;;
let def_parser s  = Parser.def_parser Lexer.read (Lexing.from_string s) ;;
let type_parser s  = Parser.type_parser Lexer.read (Lexing.from_string s) ;;
let scan s = Lexer.read (Lexing.from_string s);;

let env = [("X",Clos(Integer(5),[]));("Y",Clos(Bool(true),[]))];;
let g = [("X",Tint);("Y",Tbool)]

let ten = exp_parser "10" ;;
let seven = exp_parser "7" ;;
let solve x = getAnswer (exp_parser x ) env;;


let e = (exp_parser "\\X:Tint.Y" );;
let t = Tfunc (Tint, Tbool);;
hastype g e t;;



let e = exp_parser "let def X:Tbool = Y in X end";;
hastype g e Tint;;
hastype g e Tbool;;


let s1 = "\\X:(Tint).(let def A:Tint = X+1 in let def B:Tint = X+3 in if cmp X then A + B else A -B fi end end)"
let e1 = exp_parser s1 ;;
hastype g (App(e1,ten)) Tbool;;
hastype g (App(e1,ten)) Tint;;
getAnswer (App(e1,ten)) env;;



let s1= " rec(Fib:Tint)->X:Tint.(if (X=0) \\/ (X = 1) then X else Fib(X-1) + Fib(X-2) fi ) "
let fib= exp_parser s1
let t1 = Tfunc(Tint,Tint);;
hastype [] fib t1;;
hastype g fib t1;;
hastype g fib (Tfunc(Tbool,Tint));;

let s2 = " rec(Gcd:Tint)->Y:(Tint*Tint).(let def A:Tint = proj(1,2)Y in let def B:Tint = proj(2,2)Y in if (B = 0)then A else Gcd((B, A mod B)) fi end end )"
let gcd = exp_parser s2
let t2 = Tfunc( Ttuple[Tint;Tint], Tint );;
hastype [] gcd t2;;
hastype g gcd t2;;


(*to check the working of fibonnaci*)
let getFib x = getAnswer (App(fib,exp_parser x )) env;;
getFib "0";;
getFib "1";;
getFib "2";;
getFib "3";;
getFib "4";;
getFib "5";;
getFib "6";;
getFib "7";;
getFib "8";;
getFib "9";;

(*to check the working of gcd*)
let getGCD x = getAnswer (App(gcd,(exp_parser x ))) env;;
getGCD "(144,25)";;
getGCD "(25,144)";;
getGCD "(71,908)";;
getGCD "(908,71)";;
getGCD "(124,6)";;
getGCD "(6,124)";;
getGCD "(12,24)";;
getGCD "(24,12)";;
getGCD "(63,84)";;
getGCD "(84,63)";;

let s3 = "let def X:Tint = 1  in
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
          end";;
(*to check the scoping*)
let e3 = exp_parser s3 ;;
hastype [] e3 Tint;; (*should give true *)
hastype [("X",Tbool)] e3 Tint;; (*should give true*)

let s4 ="let def Y:Tint = 1  in
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
          end";;

let e4 = exp_parser s4 ;;
hastype [] e4 Tint;; (*should give error due to scoping*)
hastype g e4 Tint;;
getAnswer e4 env;;(*should 26*)

let s5 = "proj(2,2)(
       if not cmp X
            then
                (  X+4,
                   let def X:(Tint*Tint) = (X,X-1) in ( proj(2,2)X + proj(1,2)X,T ) end
                )

             else
                    (0,(0,F))
        fi

)";;
let e5 = exp_parser s5;;
hastype [] e5 (Ttuple[Tint;Tbool]);;
hastype  g e5 (Ttuple[Tint;Tbool]);;
getAnswer e5 env;;

let s6= " rec(Fib1:Tint)->X:Tint.(

                (rec(Fib:Tint)->X:Tint.
                                  (if (X=0) \\/ (X = 1) then X else Fib(X-1) + Fib(X-2) fi
                                  )
                 )(X)
           )
"
let fib1 = exp_parser s6 ;;
let getFib1 x = getAnswer (App(fib1,exp_parser x )) env;;
getFib1 "0";;
getFib1 "1";;
getFib1 "2";;
getFib1 "3";;
getFib1 "4";;
getFib1 "5";;
getFib1 "6";;
getFib1 "7";;
getFib1 "8";;
getFib1 "9";;


let s7 = "let def Z:Tint =
             let def X:Tint =
                (rec(Fib:Tint)->X:Tint.(if (X=0) \\/ (X = 1) then X else Fib(X-1) + Fib(X-2) fi))(10)
              in
                  X
              end

          in
              Fib(7)
          end

" ;;
let e7 = exp_parser s7;;
hastype g e Tint;; (*false because scope of Fib ends*)
(*getAnswer e7 env will give ElementNotFoundException since scope of Fib would have ended*)
