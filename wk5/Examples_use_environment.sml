val x = 99
val y = true
val z = 2.3 	

datatype result = I of int | B of bool | R of real

datatype env    = Env of (string -> result)

exception  Error of string
exception  UnboundID of string
exception  Not_implemented

fun emptyenvFun  (x : string) : result = raise UnboundID x;

val emptyenv = Env emptyenvFun

fun look_up (Env e) x = e x
	
val env_19 = fn "x" => I 99
             |  "y" => B true
             |  "z" => R 2.3
             |   w   => raise UnboundID w

val x_19 = look_up (Env env_19) "x"
val y_19 = look_up (Env env_19) "y"
val z_19 = look_up (Env env_19) "z"

val x = true

val env_28 = fn "x" => B true
                |  w  => look_up (Env env_19) w 

val x_28 = look_up (Env env_28) "x"

(* e [x=v]  update e x v *)

(*  update : env -> string -> result -> string -> result  *)
fun update env (x : string) (value : result) =
                  fn y => if x = y then value else look_up env y

