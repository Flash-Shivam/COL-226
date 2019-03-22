(* Dummy implementation of A1 *)
open A0
exception Illformedstack;;

(* abstract syntax *)
type  exptree =
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)

(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)



let rec pro i l = match l with
| xs :: xl -> if(i>1 ) then (pro (i-1) xl) else xs;;

let gi q = match q with
| NumVal(i) -> i;;


let gb r = match r with
| BoolVal(i) -> i;;

let rec eval ex rho = match ex with
| Var s -> (rho s)
| N(i) -> NumVal(i)
| B(i) -> BoolVal(i)
| Add(t1,t2) -> NumVal((gi (eval t1 rho)) + (gi (eval t2 rho)))
| Sub(t1,t2) -> NumVal((gi (eval t1 rho)) - (gi (eval t2 rho)))
| Mult(t1,t2) -> NumVal((gi (eval t1 rho))*(gi (eval t2 rho)))
| Div(t1,t2) -> NumVal((gi (eval t1 rho))/(gi (eval t2 rho)))
| Rem(t1,t2) -> NumVal((gi (eval t1 rho)) mod (gi (eval t2 rho)))
| Abs(t1) -> if((gi (eval t1 rho)) > 0) then NumVal((gi (eval t1 rho))) else NumVal(-1*(gi (eval t1 rho)))
| Negative(t1) -> NumVal(-1*(gi (eval t1 rho)))
| Not(t1) ->  BoolVal( not (gb (eval t1 rho)))
| Equals(t1,t2) -> if( (gi (eval t1 rho)) - (gi (eval t2 rho)) = 0 ) then BoolVal(true) else BoolVal(false)
| Conjunction(t1,t2) -> BoolVal( (gb (eval t1 rho)) && gb ((eval t2 rho)))
| Disjunction(t1,t2) -> BoolVal( (gb (eval t1 rho)) || gb ((eval t2 rho)))
| LessT(t1,t2) -> if((gi (eval t1 rho)) - (gi (eval t2 rho)) < 0) then BoolVal(true) else BoolVal(false)
| GreaterT(t1,t2) -> if((gi (eval t1 rho)) - (gi (eval t2 rho)) > 0) then BoolVal(true) else BoolVal(false)
| GreaterTE(t1,t2) -> if((gi (eval t1 rho)) - (gi (eval t2 rho)) >= 0) then BoolVal(true) else BoolVal(false)
| LessTE(t1,t2) ->  if((gi (eval t1 rho)) - (gi (eval t2 rho)) <= 0) then BoolVal(false) else BoolVal(true)
| InParen(t1) -> (eval t1 rho)
| IfThenElse(t1,t2,t3) -> if ( (gb (eval t1 rho) )) then (eval t2 rho) else (eval t3 rho)
| Tuple(i,tl) -> TupVal(i,(x tl rho))
| Project((i,n),tl) ->( match (eval tl rho) with TupVal(r,s) -> (pro i s)  )  
and x l rho= match l with
| [] -> []
| xs::xl -> (eval xs rho)::(x xl rho) ;;

let stackmcc l = match l with
| [] -> Num(NonNeg,[0])
| hd :: tl -> hd ;;

let rec ram i l = match l with
| [] -> []
| hd::tl -> if(i>0) then  (ram (i-1) tl)@[hd]  else  [];;

let rec sita i l = match l with 
| [] -> []
| hd::tl -> if(i>1) then (sita (i-1) tl) else tl;;




let getBi f = match f with 
| Num(i) -> i;;

let rec stackmc s binding pgm = match pgm with
| [] -> stackmcc s
| VAR(i) :: l1 -> stackmc ((binding i)::s) binding l1
| NCONST(i) :: l1 -> stackmc (Num(i)::s) binding l1
| BCONST(i) :: l1 -> stackmc (Bool(i)::s) binding l1
| MULT :: l1 -> (match s with
(x :: (y :: r)) -> stackmc (Num(mult (getBi x)  (getBi y)) :: r) binding l1)
| PLUS :: l1 ->( match s with
(x :: (y :: r)) -> stackmc (Num(add (getBi x)  (getBi y)) :: r) binding l1)
| MINUS :: l1 -> (match s with
(x :: (y :: r)) -> stackmc (Num(sub (getBi y)  (getBi x)) :: r) binding l1)
| DIV :: l1 -> (match s with
(x :: (y :: r)) -> stackmc (Num(div (getBi y)  (getBi x)) :: r) binding l1)
| REM :: l1 -> (match s with
(x :: (y :: r)) -> stackmc (Num(rem (getBi y)  (getBi x)) :: r) binding l1)
| ABS :: l1 -> ( match s with
(x :: r) -> stackmc (Num(abs (getBi x) ) :: r) binding l1)
| UNARYMINUS :: l1 -> ( match s with
(x :: r) -> stackmc (Num(minus (getBi x) ) :: r) binding l1)
| NOT :: l1 -> ( match s with
(x :: r) -> if(x = Bool(true) ) then (stackmc ((Bool(false)) :: r) binding l1) else (stackmc ((Bool(true)) :: r) binding l1))
| CONJ :: l1 ->( match s with
(x :: (y :: r)) -> if(x = Bool(false) || y = Bool(false)) then (stackmc (Bool(false) :: r) binding l1) else (stackmc (Bool(true) :: r) binding l1))
| DISJ :: l1 ->( match s with
(x :: (y :: r)) -> if(x = Bool(true) || y = Bool(true)) then (stackmc (Bool(true) :: r) binding l1) else (stackmc (Bool(false) :: r) binding l1))
| EQS :: l1 -> (match s with
(x :: (y :: r)) -> stackmc (Bool(eq (getBi x)  (getBi y)) :: r) binding l1)
| GT :: l1 -> (match s with
(x :: (y :: r)) -> stackmc (Bool(gt (getBi y)  (getBi x)) :: r) binding l1)
| LT :: l1 -> (match s with
(x :: (y :: r)) -> stackmc (Bool(lt (getBi y)  (getBi x)) :: r) binding l1)
| GTE :: l1 -> (match s with
(x :: (y :: r)) -> stackmc (Bool(geq (getBi y)  (getBi x)) :: r) binding l1)
| LTE :: l1 -> (match s with
(x :: (y :: r)) -> stackmc (Bool(leq (getBi y)  (getBi x)) :: r) binding l1)
| PAREN :: l1 -> (match s with
(x :: r) -> stackmc (x :: r) binding l1)
| IFTE :: l1 -> (match s with
(x :: (y :: z :: r)) -> if (z = Bool(true) ) then (stackmc (y :: r) binding l1) else (stackmc (x :: r) binding l1))
| TUPLE(i) :: l1 -> ( stackmc ( (Tup(i,(ram i s))) :: (sita i s)) binding l1)
| PROJ(i,j) :: l1 -> ( match s with Tup(r,m)::z -> stackmc ((List.nth m (i-1))::z) binding l1)
| _ -> raise Illformedstack;;

let rec compile ex = match ex with
                        | Var s -> [VAR(s)]
                        | N(n) ->  [NCONST(mk_big (n))]
			| B(n) -> [BCONST(n)]
                        |  Add(t1,t2) -> (compile t1) @ (compile t2) @ [PLUS]
                        |  Sub(t1,t2) -> (compile t1) @ (compile t2) @ [MINUS]
                        |  Mult(t1,t2) -> (compile t1) @ (compile t2) @ [MULT]
                        |  Div(t1,t2) -> (compile t1) @ (compile t2) @ [DIV]
                        |  Rem(t1,t2) -> (compile t1) @ (compile t2) @ [REM]
                        |  Negative(t1) -> (compile t1) @ [UNARYMINUS]
                        |  Abs(t1) -> (compile t1) @ [ABS]
                        |  Not(t1) -> (compile t1) @ [NOT]
                        |  Equals(t1,t2) -> (compile t1) @ (compile t2) @ [EQS]
                        |  Conjunction(t1,t2) -> (compile t1) @ (compile t2) @ [CONJ]
                        |  Disjunction(t1,t2) -> (compile t1) @ (compile t2) @ [DISJ]
                        |  LessT(t1,t2) -> (compile t1) @ (compile t2) @ [LT]
                        | GreaterT(t1,t2) -> (compile t1) @ (compile t2) @ [GT]
                        | GreaterTE(t1,t2) -> (compile t1) @ (compile t2) @ [GTE]
                        | LessTE(t1,t2) -> (compile t1) @ (compile t2) @ [LTE]
                        | InParen(t1) -> (compile t1) @ [PAREN]
                        | IfThenElse(t1,t2,t3) -> (compile t1) @ (compile t2) @ (compile t3) @ [IFTE]
			| Tuple(i,tl) -> (tcompile tl) @ [TUPLE(i)]
			| Project((i,n),tl) -> (compile tl) @ [ PROJ(i,n)]
and tcompile l = match l with 
| [] -> []
| hd::tl -> (compile hd) @ (tcompile tl)
;;
