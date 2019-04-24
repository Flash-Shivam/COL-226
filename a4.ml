open A1
exception TypeError of string 
exception Not_implemented
let rec fit table x=
		match table with	
		[]-> failwith "Variable ID unavailable in Reference Table." 
		|(m,n)::ys -> if (x=m) then n 
					else (fit ys x)

let ad table x = table@[x];;


let rec intersect l1 l2 =
	let rec check a l =
		(match a,l with
		(t,[]) -> false
		|((s,v),(s1,v1)::xs) -> (s=s1)||(check a xs)
		| _ -> failwith "Error: Improper Lists, Please Check again.")
	in (match l1 with 
	[] -> false
	|x::xs -> (check x l2)||(intersect xs l2) ) 
	
let rec get_type (e:exptree) table =
	match e with 
	Var(x) -> (fit table x)
	| N(i) -> Tint 
	| B(i) -> Tbool
	| Abs(x) | Negative(x) -> if (get_type x table)= Tint then Tint else raise (TypeError "Error in Unary integers") 
	| Not(x) -> if (get_type x table) = Tbool then Tbool else  raise (TypeError "Error in Unary bool")
	| Add(x,y) | Sub(x,y) | Mult(x,y) | Div(x,y) | Rem(x,y) -> if (get_type x table)= Tint && (get_type y table)=Tint then Tint else raise (TypeError "Error in Binary integers")
	| Conjunction(x,y) | Disjunction(x,y) -> if (get_type x table)=Tbool && (get_type y table)=Tbool then Tbool else raise (TypeError "Error in Binary bool")
	| Equals(x,y) |GreaterTE(x,y) |GreaterT(x,y) |LessTE(x,y) |LessT(x,y) -> if (get_type x table)=Tint && (get_type y table)=Tint then Tbool else raise (TypeError "Error in Binary Integers to bool")
	| InParen(x) -> (get_type x table)
	| IfThenElse(x,y,z) -> if (get_type x table) = Tbool && (get_type y table)=(get_type z table) then (get_type y table) else raise (TypeError "Error in IfThenElse")
	| Tuple(x,l) ->
		let rec compl l = match l with 
		[] -> []
		|b::bs -> [(get_type b table)]@(compl bs)
		in (Ttuple (compl l))
	| Project((i,n),x) -> 
		(match (get_type x table) with
		Ttuple(m) -> (List.nth m (i-1))
		|_ -> failwith "Error: Next to Project is not a Ttuple")
	| Let((a:definition),(b:exptree))-> get_type b ((ym table a)@table)  
	| FunctionAbstraction(s,f,x) -> Tfunc(f, (get_type x (ad table (s,f))))
	| FunctionCall(s,x) -> match (get_type s table) with	
					Tfunc(a,b) -> if(a=(get_type x table)) then b
							else failwith "Error: Argument of Function does not match."
					|_ -> failwith "Error: In Function Call, First Expression is not a proper Function." 
	
	
and ym t (d:definition) = match d with 
	Simple(s,f,e) -> [(s,f)]
	|Sequence(dlist) ->(match dlist with
		[]->[]
		|x::xs -> ((ym ((ym t x)@t) (Sequence(xs)))@(ym t x)))
	|Parallel(dlist) ->(match dlist with
		[]->[]
		|x::xs -> if(intersect (ym t x) (ym t (Parallel(xs)))) then failwith "Error: Simultaneous Assignments in Parallel." else
		((ym t (Parallel(xs)))@(ym t x)))
	|Local(d1,d) -> (ym ((ym t d1)@t) d) ;;
	
(* ym = get new definitions in form of list *)
let refine l = 
	let rec reflocal l =
		(match l with 
			[] -> []
			|x::xs -> if( intersect [x] (reflocal xs)) then (reflocal xs)
					else [x]@(reflocal xs))
	in (reflocal (List.rev l)) ;;

let get_table t d = (refine (ym t d)) ;;
	
let hastype g e t =
	if (get_type e g)=t then true
	else false ;;

let compa x y = (match x,y with
	((s1,v1),(s2,v2)) -> (compare s1 s2)
	|_ -> 0);;
	
let rec yields g d g_dash = match d with 
	Simple(s,f,e) -> if(( f = (get_type e g))&&(List.sort compa (get_table g d) = List.sort compa g_dash)) then true else false
| _ ->  (List.sort compa (get_table g d) = List.sort compa g_dash) ;;
	
	 
