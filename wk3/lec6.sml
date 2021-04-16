(* Reminder: the problem with free variable *)
(* Example A *)

print "Free Variables \n\n";

val x = 1
fun f y = x + y
val x = 2
val y = 3
val z = f(x+y);
val w = 99;
val x = 100;
val z1 = f 99;

print "\n\n Returning a function \n\n";

(* Return a function *)
val x_a = 1 
fun f y = let 
              val x_a = y+1 
              fun foo q = x_a + y + q
           in foo
            end 
          
val x_a = 3 
val g = f x_a
val y = 5
val z_a = g 6;

(* Pass a function *)
print "\n\n Passing a function \n\n";

fun f g = let 
            val x = 3 
          in g 2 end
val x = 4
fun h y = x + y
val z = f h;


(* Currying *)
print "\n\n Currying  \n\n";

(*
fun filter (f, [])    = []
|   filter (f, x::xs) = if f x then x::(filter (f,xs))
                               else filter (f,xs)

val x7 = filter( fn x => (x mod 2 = 0 ), [2,3,4,5,6,7]);
*)

fun filter f []      = []
|   filter f (x::xs) = if f x then x::(filter f xs)
                               else filter f xs

val only_positives = filter (fn x => 0 < x)

val only_even = filter (fn x => (x mod 2 = 0));

(* Advantages of static scope:
      - renaming
      - dead code
      - avoid recomputation
*)

(* Renaming. Are functions f and f1 the same? *)

print "\n\n Renaming  \n\n";

fun f y = let val x = y + 1 
           in fn q => x + y + q
          end

fun f1 y = let val w = y + 1 
           in fn q => w + y + q
          end

val test_f  = f  99 0;
val test_f1 = f1 99 0;

(*  Dead code elimination 
    Functions h and h1 are the same, no matter 
    what argument is passed in 
*)

print "\n\n DCE  \n\n";

fun h g = let val x = 3 
           in g 2
          end

fun h1 g = g 2
  
val x = 2 
val test_h = h  (fn z => x*z)
val test_g = h1 (fn z => x*z);

(************************************************)

print "\n\n Avoid recomputation \n\n";

fun all_shorter (xs,s) = 
   List.filter (fn x=>String.size x < String.size s) xs;

fun all_shorter1 (xs,s) = 
  let val n = String.size s
   in List.filter (fn x=>String.size x < n) xs
  end

val test_all_shorter = all_shorter(["a","ab","abc","abcd","abcde"],"1234")
(************************************************)




