datatype Term = NUM of int | DIVIDE of Term * Term

exception DivZero;

datatype result = RES_NUM of int | ERROR

(* interp : Term -> int *)

fun interp (NUM n) = n
  |	interp (DIVIDE(t1, t2)) = (case (interp t1, interp t2) of
  								   (n1, 0)  => raise DivZero
  							  |	 (n1, n2) => n1 div n2)

val x = interp (DIVIDE (NUM 3, NUM 0)) handle DivZero => 0


(* interp : Term -> result *)

fun interp (NUM n) = RES_NUM n
  |	interp (DIVIDE(t1, t2)) = (case (interp t1, interp t2) of
  								   (RES_NUM n1, RES_NUM 0)  => ERROR
  							  |	 (RES_NUM n1, RES_NUM n2) => RES_NUM (n1 div n2)
  							  |  _ => ERROR)

val x' = case interp (DIVIDE (NUM 3, NUM 0)) of
	  RES_NUM n => n
  |	ERROR => 0


val count = ref 0;

fun interp_state (NUM x) = x
|   interp_state (DIVIDE (t1, t2)) = let val _ = (count := !count + 1)
                                     in (interp_state t1) div (interp_state t2)
                                     end

val div_state = interp_state (DIVIDE (DIVIDE (NUM 3, NUM 1), NUM 1));   

(* interp_state : term -> int * int *)

fun interp_state (NUM x) = (x, 0)
 |	interp_state (DIVIDE(t1, t2)) = (case (interp_state t1, interp_state t2) of
 										((n1, c1), (n2, c2)) => (n1 div n2, c1 + c2 + 1))


val div_state' = interp_state (DIVIDE (DIVIDE (NUM 3, NUM 1), NUM 1));                                     