type sign = Neg | NonNeg ;;
type bigint = sign * int list ;;
exception DivisionByZero;;
(* c is a function which converts int num to list with MSB as first element of list *)
let rec c x l = match x with
| 0 -> l
| _ -> c (x/10) (x mod 10 :: l);;

let mk_big x = match x with
| 0 -> (NonNeg,[])
| _ when x < 0 -> (Neg, c (-1*x) [])
| _ -> (NonNeg, c x []);;

(* this function converts (_,[0]) -> (NonNeg,[])  *)
let fz (x,l) = if (l = [0]) then (NonNeg,[]) else (x,l);;

let ab (x,l) = match x with
_ -> (NonNeg,l);;

let minus (x,l) = match x with
| Neg -> (NonNeg,l)
| NonNeg -> if(l=[]) then (NonNeg,[]) else (Neg,l) ;;

let abs (x,l) = match x with
| _ -> (NonNeg,l);;

(* function p converst int list to string *)
let rec p (x,l) = match l with
| [] -> ""
| hd :: tl  -> string_of_int(hd)^p(x,tl);;

let print_num (z,m) = match z with
| Neg ->  "-"^(p (z,m))
| NonNeg -> p (z,m);;

(* checks two lists of same length whether there elements are identical *)
let rec z (x,l1) (y,l2) = match l1 with
| [] -> true
| hd :: tl -> match l2 with
| [] -> true
| r :: s -> if (r=hd) then z (x,tl) (y,s) else false;;

(* func g,gq,l,lq works only for List.length l1 = List.length l2*)
let rec gq (x,l1) (y,l2) = match l1 with
| [] -> false
| hd :: tl -> match l2 with
| [] -> true
| r :: s -> if (r<hd) then true
else if (r=hd) then gq (x,tl) (y,s)
else false;;

let g (x,l1) (y,l2) = if(gq (x,l1) (y,l2)&& l1 = l2) then false else gq (x,l1) (y,l2);;

let rec lq (x,l1) (y,l2) = match l1 with
| [] -> true
| hd :: tl -> match l2 with
| [] -> false
| r :: s -> if (r>hd) then true
else if(r=hd)then lq (x,tl) (y,s)
else false;;

let l (x,l1) (y,l2) = if(lq (x,l1) (y,l2)&& l1 = l2) then false else lq (x,l1) (y,l2);;

let eq (x,l1) (y,l2) = if (x = y && List.length l1 = List.length l2) then z (x,l1) (y,l2) else false;;

let geq (x,l1) (y,l2) = if ( x = NonNeg && y = Neg ) then true else
if (( x = y && x = NonNeg )&& ( List.length l1 > List.length l2)) then true else
if (( x = y && x = NonNeg )&& ( List.length l1 = List.length l2)) then gq (x,l1) (y,l2) else
if (( x = y && x = Neg )&& ( List.length l1 < List.length l2)) then true else
if (( x = y && x = Neg )&& ( List.length l1 = List.length l2)) then lq (x,l1) (y,l2) else false ;;

let leq (x,l1) (y,l2) = if (x = Neg && y= NonNeg) then true else
if ((x = y && x = NonNeg)&&(List.length l1 < List.length l2) ) then true else
if ((x = y && x = NonNeg)&&(List.length l1 = List.length l2) ) then lq (x,l1) (y,l2) else
if ((x = y && x = Neg )&& ( List.length l1 > List.length l2)) then true else
if ((x = y && x = Neg )&& ( List.length l1 = List.length l2)) then gq (x,l1) (y,l2) else false ;;

let gt (x,l1) (y,l2) = if ( x = NonNeg && y = Neg ) then true else
if (( x = y && x = NonNeg )&& ( List.length l1 > List.length l2)) then true else
if (( x = y && x = NonNeg )&& ( List.length l1 = List.length l2)) then g (x,l1) (y,l2) else
if (( x = y && x = Neg )&& ( List.length l1 < List.length l2)) then true else
if (( x = y && x = Neg )&& ( List.length l1 = List.length l2)) then l (x,l1) (y,l2) else false ;;

let lt (x,l1) (y,l2) = if (x = Neg && y= NonNeg) then true else
if ((x = y && x = NonNeg)&&(List.length l1 < List.length l2) ) then true else
if ((x = y && x = NonNeg)&&(List.length l1 = List.length l2) ) then l (x,l1) (y,l2) else
if ((x = y && x = Neg )&& ( List.length l1 > List.length l2)) then true else
if ((x = y && x = Neg )&& ( List.length l1 = List.length l2)) then g (x,l1) (y,l2) else false ;;

(*this functions reverses the elements of list  *)
let rec rev l = match l with
| [] -> []
| hd :: tl ->  rev tl @ [hd] ;;

(* this function returns the list without preceding zeroes  *)
let rec rz l = match l with
|  [] -> []
| 0 :: tl -> rz tl
| a -> a ;;

(* this functions adds the two input lists *)
let rec ad l1 cr l l2 = match (l1,l2) with
| ([],[]) -> cr :: l
| ([],hd::tl) -> ad [] ((hd+cr)/10) ((hd+cr)mod 10 :: l) tl
| (hd::tl,[]) ->  ad tl ((hd+cr)/10) ((hd+cr)mod 10 :: l) []
| (hd::tl,r::s) ->  ad tl ((hd+cr+r)/10) ((r+hd+cr)mod 10 :: l) s;;

 (* works only if List.Length l1 >= List.Length l2*)
  let rec sb l1 cr l l2 = match (l1,l2) with
  | ([],[]) -> cr :: l
  | (hd::tl,[]) ->  if (hd+cr>=0) then sb tl (0) ((hd+cr) :: l) [] else sb tl (-1) ((10+hd+cr) :: l) []
  | (hd::tl,r::s) -> if (hd+cr>=r) then sb tl (0) ((-r+hd+cr) :: l) s else sb tl (-1) ((10-r+hd+cr)mod 10 :: l) s;;

let sub (x,l1) (y,l2) = if(eq (x,l1) (y,l2)) then (NonNeg,[]) else
if (geq (x,l1) (y,l2) && x = NonNeg && y = NonNeg) then fz (NonNeg,rz (sb (rev l1) (0) [] (rev l2))) else
if (geq (x,l1) (y,l2) && x = NonNeg && y = Neg) then fz (NonNeg,rz (ad (rev l1) (0) [] (rev l2))) else
if (geq (x,l1) (y,l2) && x = Neg && y = Neg) then fz (NonNeg,rz (sb (rev l2) (0) [] (rev l1))) else
if (leq (x,l1) (y,l2) && x = Neg && y = NonNeg) then fz (Neg,rz (ad (rev l1) (0) [] (rev l2))) else
if (leq (x,l1) (y,l2) && x = Neg && y = Neg) then fz (Neg,rz (sb (rev l1) (0) [] (rev l2))) else
 fz (Neg,rz (sb (rev l2) (0) [] (rev l1)));;

 let add (x,l1) (y,l2) = if( x = NonNeg && y = NonNeg) then fz (NonNeg,rz (ad (rev l1) (0) [] (rev l2))) else
 if( x = Neg && y = Neg) then fz (Neg,rz (ad (rev l1) (0) [] (rev l2))) else
if( x = NonNeg && y = Neg ) then fz (sub (x,l1) (NonNeg,l2)) else
 fz (sub (y,l2) (NonNeg,l1));;

(* this function returns the list which is the result of product of list l1 and g*)
let rec mm l1 c l g  = match l1 with
| [] -> c :: l
| hd :: tl ->  mm tl ((hd*g+c)/10) (((hd*g + c) mod 10) :: l) g ;;

(* this function returns list which is obtained by l1 multiplied by l2 but it has preceding zeroes and additional zero at end*)
let rec mt (x,l1) (y,l2) = match l2 with
| [] -> []
| hd :: tl -> (ad (rev (rz (mm (rev l1) (0) [] hd))) (0) [] (rev (mt (x,l1) (y,tl))))@[0]  ;;

(* this function removes a zero from the front of the list if it is present*)
let v l = match l with
| [] -> []
| hd :: tl -> if (hd = 0) then tl else l;;

let mult (x,l1) (y,l2) = match (x,y) with
| (NonNeg,Neg) -> if(l1=[])then (NonNeg,[]) else fz (Neg,rev( v( rev (rz (mt (x,l1) (y,rev (l2)))))))
|(Neg,NonNeg) -> if(l2=[])then (NonNeg,[]) else fz (Neg,rev( v( rev (rz (mt (x,l1) (y,rev (l2)))))))
| (Neg,Neg)  -> fz (NonNeg,rev( v( rev (rz (mt (x,l1) (y,rev (l2)))))))
| (NonNeg,NonNeg) -> if(l1=[]||l2=[])then (NonNeg,[]) else fz (NonNeg,rev( v( rev (rz (mt (x,l1) (y,rev (l2)))))));;

(*this function returns l1 - l2 *)
let r l1 l2 = if(l1 = l2 ) then [0] else rz (sb (rev l1) (0) [] (rev l2)) ;;

(* this function makes a list all elements zero and the length of the list is equal to quotient when l1 / l2  *)
let rec eg l1 l2  = if(geq (NonNeg,l1) (NonNeg,l2)) then 0:: (eg (r l1 l2) l2) else [0];;

(* same as normal div function but does not exception of DivisionByZero and works only for numbers of 6 - digit *)
let divv (x,l1) (y,l2) = match (x,y) with
| (NonNeg,Neg) -> if((eg l1 l2) = []) then (Neg,[0]) else (Neg,c (List.length (eg l1 l2)) [])
|(NonNeg,NonNeg) -> if((eg l1 l2) = []) then (NonNeg,[0])
else (NonNeg, c (List.length (eg l1 l2)) [])
|(Neg,Neg) -> if((eg l1 l2) = []) then (NonNeg,[0]) else (NonNeg, c (List.length (eg l1 l2)) [])
|(Neg,NonNeg) -> if((eg l1 l2) = []) then (Neg,[0])
else (Neg, c (List.length (eg l1 l2)) []);;

(* this function finds out where is the first instance that substring starting from start of l1 is greater than l2*)
let rec xx l1 l2 l = match l1 with
| hd :: tl -> if (geq (NonNeg,l@[hd]) (NonNeg,l2)) then  l@[hd] else xx tl l2 (l@[hd])
| [] -> l ;;

(* this function gives div of l1 by l2 *)
let rec quo (x,l1) (y,l2) (z,l) = if( geq (NonNeg,l1) (NonNeg,l2) ) then quo (sub (NonNeg,l1) (mult (NonNeg,l2) (divv (NonNeg,xx (l1) (l2) []) (NonNeg,l2)) ))  (y,l2) ( add (NonNeg,l) (divv (NonNeg,xx (l1) (l2) []) (NonNeg,l2)) ) else (NonNeg,l);;

(* (_,l1) -> (Neg,l1) *)
let negg (x,l1) = if (x = NonNeg) then (Neg,l1) else (x,l1) ;;

(* same as div but without div by 0 exception *)
 let qw (x,l1) (y,l2) = match (x,y) with
 | (Neg,Neg) -> if(sub (NonNeg,l1) (mult (NonNeg,l2) (quo (x,l1) (y,l2) (NonNeg,[0]))) = (NonNeg,[0])) then quo (x,l1) (y,l2) (NonNeg,[0]) else sub (quo (x,l1) (y,l2) (NonNeg,[0])) (NonNeg,[1])
 | (NonNeg,Neg) -> if(sub (NonNeg,l1) (mult (NonNeg,l2) (quo (x,l1) (y,l2) (NonNeg,[0]))) = (NonNeg,[0])) then negg (quo (x,l1) (y,l2) (NonNeg,[0])) else negg (sub (quo (x,l1) (y,l2) (NonNeg,[0])) (NonNeg,[1]))
 | (Neg,NonNeg) -> if(sub (NonNeg,l1) (mult (NonNeg,l2) (quo (x,l1) (y,l2) (NonNeg,[0]))) = (NonNeg,[0])) then negg (quo (x,l1) (y,l2) (NonNeg,[0])) else negg (sub (quo (x,l1) (y,l2) (NonNeg,[0])) (NonNeg,[1]))
 | (NonNeg,NonNeg) -> if(sub (NonNeg,l1) (mult (NonNeg,l2) (quo (x,l1) (y,l2) (NonNeg,[0]))) = (NonNeg,[0])) then quo (x,l1) (y,l2) (NonNeg,[0]) else sub (quo (x,l1) (y,l2) (NonNeg,[0])) (NonNeg,[1])

let diva (x,l1) (y,l2) = if (l2 = [0]) then raise DivisionByZero else qw (x,l1) (y,l2) ;;

(* this function appends the list l with p zeroes *)
let rec ap (x,l) p = match p with
| 0 -> (x,l)
| _ when p>0 -> ap (x,l@[0]) (p-1)
| _ when p<0 -> (x,l);;


let pr (x,l1) (y,l2) =  (ap (diva (NonNeg,l1) (ap ((NonNeg,l2)) (List.length l1 - List.length l2-1)) ) (List.length l1 - List.length l2-1) ) ;;

let rec dz (x,l1) (y,l2) (z,l) =     if(geq (NonNeg,l1) (NonNeg,l2) ) then dz (sub (NonNeg,l1) ( mult (NonNeg,l2) (pr (x,l1) (y,l2) )  )  )    (y,l2) (add (NonNeg,l) (pr (x,l1) (y,l2)) )
else (z,l);;

(* gives same answer as div but if l1 mod l2 !=0 if = 0 then gives ans expected -1 *)
let divs (x,l1) (y,l2) = match (x,y) with
| (NonNeg,NonNeg) -> if(gt (NonNeg,l2) (NonNeg,l1) ) then  (NonNeg,[]) else fz (dz (x,l1) (y,l2) (NonNeg,[]))
| (Neg,Neg) -> if(gt (NonNeg,l2) (NonNeg,l1)) then  (NonNeg,[]) else fz (dz (x,l1) (y,l2) (NonNeg,[]))
| (NonNeg,Neg) -> if(gt (NonNeg,l2) (NonNeg,l1)) then (NonNeg,[]) else fz (negg ( dz (x,l1) (y,l2) (NonNeg,[]) ))
| (Neg,NonNeg) ->  if(gt (NonNeg,l2) (NonNeg,l1)) then (NonNeg,[]) else fz (negg ( dz (x,l1) (y,l2) (NonNeg,[]) )) ;;


let div (x,l1) (y,l2) = if((mult (divs (x,l1) (y,l2)) (y,l2) )=  (x,l1) ||  (divs (x,l1) (y,l2)) = (NonNeg,[])  ) then divs (x,l1) (y,l2)
else if(x=y) then (add ( divs (x,l1) (y,l2) ) (NonNeg,[1]) )
else   (add ( divs (x,l1) (y,l2) ) (Neg,[1]) ) ;;

let rem (x,l1) (y,l2) = match (x,y) with
|(NonNeg,NonNeg) -> fz (sub (x,l1) (mult (y,l2) (div (x,l1) (y,l2))))
|(Neg,Neg) -> fz (sub (x,l1) (mult (y,l2) (div (x,l1) (y,l2))))
|(NonNeg,Neg) -> fz (sub (x,l1) (mult (y,l2) (div (x,l1) (y,l2))))
|(Neg,NonNeg) -> fz (sub (x,l1) (mult (y,l2) (div (x,l1) (y,l2))))
;;
