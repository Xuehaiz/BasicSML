(* Imperative *)

val counter = ref 0;

fun inc c = let val _ = counter := !counter +c
                 in !counter
                end ;

val res = (inc 1; inc 2; inc 3; inc 17; !counter);   


(* Functional: how to simulate state  *)

fun inc' c counter = (counter + c, counter+c);

val res1 = let
val init_counter = 0
val (c1,v1) = inc'  1 init_counter
val (c2,v2) = inc'  2 c1
val (c3,v3) = inc'  3 c2
val (c4,v4) = inc'  17 c3
in c4
end;






