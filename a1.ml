open A0;
exception Illformedstack;;

type  exptree =  N of int

                         |  Plus of exptree *  exptree

                         | Minus of exptree *  exptree

                        |  Mult of exptree *  exptree

                        | Div of exptree *  exptree

                        | Rem of exptree *  exptree

                        | Nega of  exptree

                        | Abs of  exptree

                        ;;

                        let rec size t = match t with
                        | N(_) -> 1
                        |Plus(t1,t2) -> 1 + size(t1) + size(t2)
                        |Minus(t1,t2) -> 1 + size(t1) + size(t2)
                        | Mult(t1,t2) -> 1+size(t1) + size(t2)
                        | Div(t1,t2) -> 1+size(t1) + size(t2)
                        | Rem(t1,t2) -> 1+size(t1) + size(t2)
                        | Nega(_) -> 1
                        | Abs(_) -> 1 ;;

                        let max x y = if(x>y) then x else y;;

                        let rec ht t = match t with
                        | N(_) -> 0
                        | Plus(t1,t2) -> 1 + max (ht (t1)) (ht (t2))
                        | Minus(t1,t2) -> 1 + max (ht (t1)) (ht (t2))
                        | Mult(t1,t2) -> 1 +  max (ht (t1)) (ht (t2))
                        | Div(t1,t2) -> 1 +  max (ht (t1)) (ht (t2))
                        | Rem(t1,t2) -> 1 +  max (ht (t1)) (ht (t2))
                        | Nega(_) -> 0
                        | Abs(_) -> 0 ;;

                        let rec eval t = match t with
                        | N(i) -> i
                        | Plus(t1,t2) -> (eval t1) + (eval t2)
                        | Minus(t1,t2) -> (eval t1) - (eval t2)
                        | Mult(t1,t2) -> (eval t1)*(eval t2)
                        | Div(t1,t2) -> (eval t1)/(eval t2)
                        | Rem(t1,t2) -> (eval t1) mod (eval t2)
                        | Nega(t1) -> (-1)*(eval t1)
                        | Abs(t1) -> if((eval t1) >=0) then (eval t1) else (-1)*(eval t1) ;;

                        type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS ;;

                        let rec compile t = match t with
                        |N(n) ->  [CONST(mk_big (n))]
                        |  Plus(t1,t2) -> (compile t1) @ (compile t2) @ [PLUS]
                        |  Minus(t1,t2) -> (compile t1) @ (compile t2) @ [MINUS]
                        |  Mult(t1,t2) -> (compile t1) @ (compile t2) @ [TIMES]
                        |  Div(t1,t2) -> (compile t1) @ (compile t2) @ [DIV]
                        |  Rem(t1,t2) -> (compile t1) @ (compile t2) @ [REM]
                        |  Nega(t1) -> (compile t1) @ [UNARYMINUS]
                        |  Abs(t1) -> (compile t1) @ [ABS] ;;


                        let stackmcc l = match l with
                        | [] -> (NonNeg,[0])
                        | hd :: tl -> hd ;;


                        let rec stackmc s l = match l with
                        | [] -> stackmcc s
                        | CONST(i) :: l1 -> stackmc (i::s) l1
                        | TIMES :: l1 -> (match s with
                        (x :: (y :: r)) -> stackmc ((mult x y) :: r) l1)
                        | PLUS :: l1 ->( match s with
                        (x :: (y :: r)) -> stackmc ((add x y) :: r) l1)
                        | MINUS :: l1 -> (match s with
                        (x :: (y :: r)) -> stackmc ((sub y x) :: r) l1)
                        | DIV :: l1 -> (match s with
                        (x :: (y :: r)) -> stackmc ((div y x ) :: r) l1)
                        | REM :: l1 -> (match s with
                        (x :: (y :: r)) -> stackmc ((rem y x) :: r) l1)
                        | ABS :: l1 -> ( match s with
                        (x :: r) -> stackmc ((abs x) :: r) l1)
                        | UNARYMINUS :: l1 -> ( match s with
                        (x :: r) -> stackmc ((minus x) :: r) l1)
                        | _ -> raise Illformedstack;;
