
(*  Tail call optimization *)


fun fact n = if n=0 then 1 else n*fact(n-1);

fun fact_i n  =  let
	               fun loop (n,acc) = if n=0 then acc 
	                         else loop(n-1, n*acc)  
	           	  in loop(n,1)
	           	  end    

(* Exceptions  *)


(*   First-class functions *)


fun double x = 2 * x;
fun inc x    = x + 1;


val a_tuple = (double, inc, double (inc 2));

val a_tuple1 = (fn x => 2*x, fn x=>x+1,
                (fn x => 2*x)((fn x=>x+1) 2));

val a_list  = [double, inc];

val eighteen = (#1 a_tuple) 9


 (*  Functions as Arguments *)
 (*  Iterators: Map, Filter, Fold  *)

fun map (f,xs) =
    case xs of
        [] => []
      | x::xs' => (f x)::(map(f,xs'))


fun map (f, [] )   = []
|   map (f, x::xs) = (f x)::(map (f,xs))


val x4 = map ((fn x => x+1), [4,8,12,16])

val x5 = map (hd, [[1,2],[3,4],[5,6,7]])

fun is_even 0 =  true
|   is_even 1 =  false
|   is_even x =  is_even (x-2);


val x6 = map(is_even, [4,8,13,16]);


(*   FILTER *)

fun filter (f, [])    = []
|   filter (f, x::xs) = if f x then x::(filter (f,xs))
                               else filter (f,xs)

val x7 = filter( fn x => (x mod 2 = 0 ), [2,3,4,5,6,7]);

(* Returning a function - Functions in output  *)

fun double_or_triple f =
    if f 7
    then fn x => 2*x
    else fn x => 3*x

val dbl    = double_or_triple (fn x => x-3 = 4);
val triple = double_or_triple (fn x => x = 42);

(*  Lexical scope *)

val x = 1
fun f y = x + y
val x = 2
val y = 3
val z = f(x+y);

(* Return a function *)
val x = 1
fun f y = let val x =y+1 in fn q=>x+y+q end
val x = 3
val g = f 4
val y = 5
val z = g 6

(* Pass a function *)

fun f g = let val x = 3 in g 2 end
val x = 4
fun h y = x + y
val z = f h


(* Currying *)

(*)
fun filter (f, [])    = []
|   filter (f, x::xs) = if f x then x::(filter (f,xs))
                               else filter (f,xs)

val x7 = filter( fn x => (x mod 2 = 0 ), [2,3,4,5,6,7]);
*)

fun filter f []      = []
|   filter f (x::xs) = if f x then x::(filter f xs)
                               else filter f xs

val only_positives = filter (fn x => 0 < x)

val only_even = filter (fn x => (x mod 2 = 0))



