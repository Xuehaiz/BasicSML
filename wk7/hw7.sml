(* 1 *)
(* fun f (g,h) = g (h 0) *)

(*
f : a * b -> c
g : a = d -> c
h : b = int -> d

f : (d -> c) * (int -> d) -> c
f : (a -> b) * (int -> a) -> b
*)


(* 2 *)
(* fun apply (f,x) = f x *)

(*
apply : a * b -> c
	f : a = b -> c
	x : b

apply : (b -> c) * b -> c
apply : (a -> b) * a -> b
*)

(* 3 *)
(* fun reverse nil     = nil
    |  reverse (x::xs) = reverse xs *)

(* reverse : a list -> b list *)


(* 4 *)
(* fun ff f x y = if (f x y) then (f 3 y) else (f x "zero") *)

(*
ff : a -> b -> c -> d
	 b = int
	 c = string
	 d = bool
f  : a = int -> string -> bool
x  : b = int
y  : c = string

ff : (int -> string -> bool) -> int -> string -> bool 
*)


(* 5 *)
(* fun gg f x y = if (f x y) then (f 3 y) else (f y "zero") *)
(*
gg : a -> b -> c -> d
     b = int /= string
     c = string
     d = bool
f  : a = int -> int/string -> bool
x  : b = int
y  : c = string
*)


(* 6 *)
(* fun hh f x y = if (f x y) then (f x y) else (f x "zero") *)
(*
hh : a -> b -> c -> d
     c = string
     d = bool
f  : a = b -> string -> bool
x  : b = b
y  : c = string

hh : (b -> string -> bool) -> b -> string -> bool
hh : (a -> string -> bool) -> a -> string -> bool
*)


(* fun sort(less, nil) = nil
|   sort(less, a :: l) =
	let fun insert(a, nil) = a :: nil
	      | insert(a, b :: l) = if less(a,b) then a :: (b :: l) else b :: insert(a, l)
	in
	    insert(a, sort(less, l))
	end *)


(* fun append(nil, l)    = l
  | append(x :: l, m) = append(l, m) *)


(* fun f(g,h) = g h + 2 *)


(* fun f g = g(g) + 2 *)



