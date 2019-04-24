type answer = BoolVal of bool | NumVal of int ;;

type expr = V of string | Lambda of  expr * expr | App of (expr * expr) | Plus of (expr * expr) | Mult of (expr * expr) |And of (expr * expr) | Or of (expr * expr) | Bool of bool | Integer of int | Cmp of expr | If_Then_Else of (expr * expr * expr) | Sub of (expr*expr) | Div of expr*expr | Di of expr | Mi of expr | Ad of expr | Sb of expr | Conj of expr | Disj of expr | Not of expr  | InParen of expr ;;

type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype) ;;
type table = (string * closure) list and closure = CLOSURE of (expr * table);;
exception NotFound;;

let rec find t x = match t with 
| [] -> raise(NotFound)
| hd :: tl -> ( match hd with (p,q) -> if( p = x ) then q else ( find tl x ) );;

let rec look (t:(string*closure)list) x = match t with 
| [] -> raise(NotFound)
| hd :: tl -> ( match hd with (p,a) -> if( p = x ) then a else ( look tl x ) );;

let addtable p t = [p]@t;;

let gi x = match x with NumVal(y) -> y;;

let gb x = match x with BoolVal(y) -> y;;


let rec krivinemc c s = match c with 
| CLOSURE(V(x),t) ->  krivinemc (look t x) s  
| CLOSURE(App(x,y),t) -> krivinemc (CLOSURE(x,t)) ((CLOSURE(y,t)) :: s )
| CLOSURE(Lambda(V(x),y),t) -> ( match s with 
| [] -> CLOSURE(Lambda(V(x),y),t)
|(hd :: tl) -> krivinemc (CLOSURE(y,( addtable (x,hd) (t) ))) tl )
| CLOSURE(Integer(i),t) -> CLOSURE(Integer(i),t)
| CLOSURE(Bool(b),t) -> CLOSURE(Bool(b),t)
| CLOSURE(InParen(e),t) -> (krivinemc (CLOSURE(e,t)) (s))
| CLOSURE(Di(e),t) -> ( match ( krivinemc (CLOSURE(e,t)) s) with CLOSURE(Integer(i),t) -> ( match s with CLOSURE(Integer(i1),t) :: tl -> CLOSURE(Integer(i1/i),t) ) ) 
| CLOSURE(Div(e1,e2),t) -> ( match krivinemc (CLOSURE(e1,t)) s  with CLOSURE(Integer(i1),t) -> krivinemc (CLOSURE(Di(e2),t)) (CLOSURE(Integer(i1),t) :: s) )
| CLOSURE(Mi(e),t) -> ( match ( krivinemc (CLOSURE(e,t)) s) with CLOSURE(Integer(i),t) -> ( match s with CLOSURE(Integer(i1),t) :: tl -> CLOSURE(Integer(i1*i),t) ) ) 
| CLOSURE(Mult(e1,e2),t) -> ( match krivinemc (CLOSURE(e1,t)) s  with CLOSURE(Integer(i1),t) -> krivinemc (CLOSURE(Mi(e2),t)) (CLOSURE(Integer(i1),t) :: s) )
| CLOSURE(Ad(e),t) -> ( match ( krivinemc (CLOSURE(e,t)) s) with CLOSURE(Integer(i),t) -> ( match s with CLOSURE(Integer(i1),t) :: tl -> CLOSURE(Integer(i1+i),t) ) ) 
| CLOSURE(Plus(e1,e2),t) -> ( match krivinemc (CLOSURE(e1,t)) s  with CLOSURE(Integer(i1),t) -> krivinemc (CLOSURE(Ad(e2),t)) (CLOSURE(Integer(i1),t) :: s) )
| CLOSURE(Sb(e),t) -> ( match ( krivinemc (CLOSURE(e,t)) s) with CLOSURE(Integer(i),t) -> ( match s with CLOSURE(Integer(i1),t) :: tl -> CLOSURE(Integer(i1-i),t) ) ) 
| CLOSURE(Sub(e1,e2),t) -> ( match krivinemc (CLOSURE(e1,t)) s  with CLOSURE(Integer(i1),t) -> krivinemc (CLOSURE(Sb(e2),t)) (CLOSURE(Integer(i1),t) :: s) )
| CLOSURE(Cmp(e),t) -> ( match krivinemc (CLOSURE(e,t)) s with CLOSURE(Integer(i),t) -> if(i>0) then CLOSURE(Bool(true),t) else CLOSURE(Bool(false),t) )
| CLOSURE(Not(e),t) -> ( match krivinemc (CLOSURE(e,t)) s with CLOSURE(Bool(i),t) -> if(i = false ) then CLOSURE(Bool(true),t) else CLOSURE(Bool(false),t) )
| CLOSURE(Conj(e),t) -> ( match ( krivinemc (CLOSURE(e,t)) s) with CLOSURE(Bool(i),t) -> ( match s with CLOSURE(Bool(i1),t) :: tl -> CLOSURE(Bool(i1&&i),t) ) ) 
| CLOSURE(And(e1,e2),t) -> ( match krivinemc (CLOSURE(e1,t)) s  with CLOSURE(Bool(i1),t) -> krivinemc (CLOSURE(Conj(e2),t)) (CLOSURE(Bool(i1),t) :: s) )
| CLOSURE(Disj(e),t) -> ( match ( krivinemc (CLOSURE(e,t)) s) with CLOSURE(Bool(i),t) -> ( match s with CLOSURE(Bool(i1),t) :: tl -> CLOSURE(Bool(i1||i),t) ) ) 
| CLOSURE(Or(e1,e2),t) -> ( match krivinemc (CLOSURE(e1,t)) s  with CLOSURE(Bool(i1),t) -> krivinemc (CLOSURE(Disj(e2),t)) (CLOSURE(Bool(i1),t) :: s) )
(*| CLOSURE(Mult(e1,e2),t) -> ( match ((krivinemc (CLOSURE(e1,t)) s),(krivinemc (CLOSURE(e2,t)) s)) with (CLOSURE(Integer(i1),t1),CLOSURE(Integer(i2),t2)) -> CLOSURE(Integer(i1*i2),t) )
| CLOSURE(Plus(e1,e2),t) -> ( match ((krivinemc (CLOSURE(e1,t)) s),(krivinemc (CLOSURE(e2,t)) s)) with (CLOSURE(Integer(i1),t1),CLOSURE(Integer(i2),t2)) -> CLOSURE(Integer(i1+i2),t) )
| CLOSURE(Sub(e1,e2),t) -> ( match ((krivinemc (CLOSURE(e1,t)) s),(krivinemc (CLOSURE(e2,t)) s)) with (CLOSURE(Integer(i1),t1),CLOSURE(Integer(i2),t2)) -> CLOSURE(Integer(i1-i2),t) )
| CLOSURE(And(e1,e2),t) -> ( match ((krivinemc (CLOSURE(e1,t)) s),(krivinemc (CLOSURE(e2,t)) s)) with (CLOSURE(Bool(i1),t1),CLOSURE(Bool(i2),t2)) -> CLOSURE(Bool(i1&&i2),t) )
| CLOSURE(Or(e1,e2),t) -> ( match ((krivinemc (CLOSURE(e1,t)) s),(krivinemc (CLOSURE(e2,t)) s)) with (CLOSURE(Bool(i1),t1),CLOSURE(Bool(i2),t2)) -> CLOSURE(Bool(i1||i2),t) )*)
| CLOSURE(If_Then_Else(e1,e2,e3),t) -> ( match krivinemc (CLOSURE(e1,t)) s with CLOSURE(Bool(i),t) -> if(i = true ) then (krivinemc (CLOSURE(e2,t)) s) else (krivinemc (CLOSURE(e3,t)) s) )
;;








let rec krivine e t s = match e with
| V(x) -> (find t x)
| App(x,y) -> krivine x t (( krivine y t s) :: s)
| Lambda(V(x),y) -> ( match s with (hd :: tl) -> krivine y ( addtable (x,hd) (t) ) tl )
| Bool(b) -> BoolVal(b)
| Integer(i) -> NumVal(i) 
| Cmp(e) -> if ( (gi (krivine e t s)) > 0 ) then BoolVal(true) else BoolVal(false)
| Plus(e1,e2) -> NumVal(gi (krivine e1 t s) + gi (krivine e2 t s))
| Sub(e1,e2) -> NumVal(gi (krivine e1 t s) - gi (krivine e2 t s))
| Mult(e1,e2) -> NumVal(gi (krivine e1 t s)*gi (krivine e2 t s))
| And(e1,e2) -> BoolVal((gb (krivine e1 t s))&&(gb (krivine e2 t s)))
| Or(e1,e2) -> BoolVal((gb (krivine e2 t s))||(gb (krivine e2 t s)))
| If_Then_Else(e1,e2,e3) -> if ( (gb (krivine e2 t s))) then (krivine e2 t s) else (krivine e3 t s)   
;;


