fun nats_upto (x : int) =
  let fun range ((lo : int),(hi : int)) =
    if lo < hi then
      lo :: range (lo + 1,hi)
    else
      []
  in
  range (0,x)
 end

 fun nats_upto_new (x : int) =
  let fun loop (lo : int) =
    if lo < x then
      lo :: loop (lo + 1)
    else
      []
  in
  loop 0
 end

fun bad_max (xs : int list) =
 if xs = [] then
     0 (* bad style, will fix later *)
 else if tl xs = [] then
    hd xs
 else if hd xs > bad_max (tl xs) then
   hd xs
 else
   bad_max (tl xs)

val x = bad_max (List.rev (nats_upto 50));
(* The following one takes long time...
val y = bad_max (nats_upto 50); 
*)

fun better_max (xs : int list) =
 if xs = [] then
   0 (* bad style *)
 else if List.tl xs = [] then
   List.hd xs
 else
   let val tl_max = better_max (List.tl xs) in
   if List.hd xs > tl_max then
     List.hd xs
   else
     tl_max
   end

val x_1 = better_max (List.rev (nats_upto 50));
val y_1 = better_max (nats_upto 50); 

(* Using Option type *)


fun good_max (xs : int list) =
  if xs = [] then
    NONE
  else
    let val tl_ans = good_max (tl xs) in
    if isSome tl_ans andalso valOf tl_ans > hd xs then
      tl_ans
    else 
      SOME (hd xs)
    end

(* Aliasing *)

fun sort_pair (pr : int*int)=
   if #1 pr < #2 pr then
     pr
   else
     (#2 pr, #1 pr)

fun sort_pair_1 (pr : int*int)=
   if #1 pr < #2 pr then
     (#1 pr, #2 pr)
   else
     (#2 pr, #1 pr)


fun  append ((xs: ''a list),(ys : ''a list)) =  
	if xs = []then ys else List.hd xs :: append (tl xs, ys)

val a = [1,2]
val b = [3,4]
val c = append (a,b);

(*   Data Types *)

datatype Color = Yellow | Blue | Red

val y = Yellow
val b = Blue
val r = Red

datatype Bool = True | False

datatype Student = Name of string | Year of int

(*) Notice the difference between cc1 and cc2 *)
val cc1 = "aaaa";
val cc2 = Name "aaaa"

datatype Student1 = Name of string * int| Year of int

datatype Student2 = Name of (string list) * (int -> int)| Year of int

val dd = Year 2013; (* Notice that you get the last definition
                       of the constructor *
                     *)


(* A list of length one *)
val aa = [1]; (* 1 :: [] *)
hd aa;
tl aa;

val b = NONE;
SOME 5;


