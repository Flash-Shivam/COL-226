
type expr = V of string | Lambda of  expr * expr | App of (expr * expr) | Plus of (expr * expr) | Mult of (expr * expr) |And of (expr * expr) | Or of (expr * expr) | Bool of bool | Integer of int | Cmp of expr | If_Then_Else of (expr * expr * expr) | Sub of (expr*expr) ;;

exception NotFound;;

type opcode = SUB | MULT | PLUS | APP | V of string | AND | OR | BOOL of bool | INT of int | CMP | COND of (opcode list * opcode list ) | RET | CLOS of (string * ( opcode list)) ;;

type table = (string * answer) list and vclosure = (string * (opcode list) * table) and answer = BoolVal of bool | NumVal of int | VCLOS of vclosure  ;;

let rec look (t:(string*answer)list) x = match t with 
| [] -> raise(NotFound)
| hd :: tl -> ( match hd with (p,a) -> if( p = x ) then a else ( look tl x ) );;

let addtable p t = [p]@t;;

let rec compile ex = match ex with 
| Bool(b) -> [BOOL(b)]
| V(x) -> [V(x)]
| Integer(i) -> [INT(i)]
| Lambda(V(x),e1) -> [CLOS(x,compile(e1)@[RET])] 
| App(e1,e2) -> (compile e1)@(compile e2)@[APP]
| Plus(e1,e2) -> (compile e1)@(compile e2)@[PLUS]
| Sub(e1,e2) -> (compile e1)@(compile e2)@[SUB]
| Mult(e1,e2) -> (compile e1)@(compile e2)@[MULT]
| And(e1,e2) -> (compile e1)@(compile e2)@[AND]
| Or(e1,e2) -> (compile e1)@(compile e2)@[OR]
| Cmp(e1) -> (compile e1)@[CMP]
| If_Then_Else(e1,e2,e3) -> (compile e1)@[COND(compile e2,compile e3)]
;;


let rec secd_mc (s:answer list) (e:table) (c:opcode list) (d:((answer list)*(table)*(opcode list)) list ) = match c with 
| INT(i) :: c1 -> secd_mc (NumVal(i)::s) e c1 d
| BOOL(b) :: c1 -> secd_mc (BoolVal(b)::s) e c1 d
| V(x) :: c1 -> secd_mc ((look e x) :: s) e (c1) d
| CLOS(x,c1) :: c2 -> secd_mc (VCLOS(x,c1,e) :: s) e (c2) d
| AND :: c1 -> (match s with (BoolVal(i1) :: (BoolVal(i2) :: s1)) -> secd_mc (BoolVal(i1&&i2) :: s1) e c1 d )
| OR :: c1 -> (match s with (BoolVal(i1) :: (BoolVal(i2) :: s1)) -> secd_mc (BoolVal(i1||i2) :: s1) e c1 d )
| PLUS :: c1 -> (match s with (NumVal(i1) :: (NumVal(i2) :: s1)) -> secd_mc (NumVal(i1+i2) :: s1) e c1 d )
| CMP :: c1 -> ( match s with (NumVal(i) :: s1) -> if( i > 0) then (secd_mc (BoolVal(true) :: s1) e c1 d ) else (secd_mc (BoolVal(false) :: s1) e c1 d ))
| SUB :: c1 -> (match s with (NumVal(i1) :: (NumVal(i2) :: s1)) -> secd_mc (NumVal(-i1+i2) :: s1) e c1 d )
| MULT :: c1 -> (match s with (NumVal(i1) :: (NumVal(i2) :: s1)) -> secd_mc (NumVal(i1*i2) :: s1) e c1 d )
| APP :: c1 -> ( match s with (a :: (VCLOS(x,c2,e1) :: s1)) -> secd_mc ([]) (addtable (x,a) (e1)) (c2) ((s1,e,c1) :: (d)) )
| RET :: c1 -> ( match d with ((s',e',c') :: (d1)) -> (match  s with v :: s1 -> secd_mc (v::s') (e') (c') (d1) ) )
| COND(e1,e2) :: c1 -> ( match s with (BoolVal(true) :: s1 ) -> secd_mc (s1) (e) (e1@c1) (d)
| (BoolVal(false) :: s1 ) -> secd_mc (s1) (e) (e2@c1) (d) )
| [] -> ( match s with hd :: tl -> hd )
;;
 
let execute ex e = secd_mc [] e (compile ex) [] ;;
