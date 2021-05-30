fun f x y = (x = y) ;
  ''a -> ''a -> bool

  f 2 3;
  f true false;
  f (fn x=>x) (fn x=>x)

fun g x y = x < y; 


fun insert (x,[])     = [x]
|   insert (x, y::ys) = if x < y then x :: y :: ys
                          else y :: (insert (x, ys));

fun sort [] = []
|   sort (x::xs) = insert (x,  (sort xs));



val sort_n = sort [4,9,1];

val sort_b = sort [false, true];
val sort_s = sort ["hi","bye","ciao"];




datatype 'a tree = Leaf of 'a | Node of ('a tree * 'a tree)

fun fringe (Leaf a) = [a]
|   fringe (Node (x, y)) = (fringe x) @ (fringe y);


fun samefringe t1 t2 = (fringe t1) = (fringe t2);


val test1 = Node (Node (Leaf 1, Leaf 2), Leaf  3 );
val test2 = Node (Leaf 1, Node (Leaf 2, Leaf 3));
val test3 = Node (Leaf 4, Node (Leaf 2,Leaf 3));
val res1 = samefringe test1 test2
val res2 = samefringe test1 test3
val res3 = samefringe test2 test3