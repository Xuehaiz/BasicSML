(* CSE 341, Homework 2 Tests *)

use "hw2.sml";

(* You will surely want to add more! *)

(* warning: because real is not an eqtype, json is not an eqtype, so you cannot 
   use = on anything including something of type json.
   See test1, test3, and test9 for examples of how to work around this. *)

val epsilon = 0.0001
fun check_real (r1,r2) = Real.abs (r1 - r2) < epsilon

val test1 =
    case make_silly_json 2 of
        Array [Object [("n",Num x),
                       ("b",True)],
               Object [("n",Num y),
                       ("b",True)]]
        => check_real (x,2.0) andalso check_real(y,1.0)
      | _ => false


val test2 = assoc ("foo", [("bar",17),("foo",19)]) = SOME 19

val test3 = case dot (json_obj, "ok") of SOME True => true |  _ => false