(* Delayed Evaluation *)
 
fun double x = x + x; 
val test2 = double (print "Hello in test2 \n";5+1);

fun double' x = x () + x();
val test3 = double' (fn ()=>(print ("Hello in test3 \n");5+1));

fun factorial n = if n=0 then 1 else n*factorial(n-1);

fun my_if e1 e2 e3 = if e1 then e2 else e3;

val test3 = my_if true (1+1) (2+3);

fun factorial' n = my_if (n=0) 1 (n*factorial'(n-1));

(* val test4 = factorial' 3;*)

fun my_if_good e1 e2 e3 = if e1 then e2() else e3();

fun factorial_good n = my_if_good (n=0) (fn ()=>1) (fn () => n*factorial_good (n-1));

val test5 = factorial_good 3;

fun mult 0 y = 0
|   mult 1 y = y
|   mult x y = y + (mult (x-1) y);

val test6 = mult 0 (print "I am executing factorial of 5\n";factorial 5);

fun mult' 0 y_thunk = 0
|   mult' 1 y_thunk = y_thunk()
|   mult' x y_thunk = y_thunk() + (mult' (x-1) y_thunk);

val test7 = mult' 0 (fn ()=> (print "I am executing factorial of 100";factorial 100));

val test7a = mult' 3 (fn ()=> (print "I am doing the multiplication\n";2));

(* mul' x y  is great for x=1, okay for x=1, bad for x > 1 because we evaluate y for 
   every recursive call
 *)

(* Lazy evaluation:   use mutation to remember the result of the computation *)

datatype thunk = Done of int | Not_Done of unit -> int

fun delay thunk =  ref(Not_Done thunk);


fun force p = case !p of
                     (Done v) => v
                   | (Not_Done t) => let val v = t ()
                                            in (p:= Done v;v)
                                          end;

fun f x = (force x) + (force x);

val test8 = f (delay (fn ()=> (print("Hello\n"); 1 + 2 )));

(* Streams *)

(* How to define the infinite list of ones *)

fun ones () = 1 :: ones (); 

fun nats n = n :: (nats (n+1));
val test11 = nats 0 ;

datatype newList = Cons of int * (unit->newList) ;

fun one_seq () = Cons (1,one_seq);

fun take 1 (Cons (x,s)) = x
|   take n (Cons (x,s)) = take (n-1) (s());

val test10 = take 100 (one_seq ());
