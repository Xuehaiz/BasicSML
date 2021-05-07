(*  Concrete syntax
e :: x | n | true | false | succ | pred | iszero | 
   | + e e  
   | if e then e else e
   | fn x => e | e e | (e) | let x = e in e

*)

datatype term = AST_ID of string | AST_NUM of int  | AST_BOOL of bool|
                AST_SUCC | AST_PRED | AST_ISZERO | AST_ADD |
                AST_IF of term * term * term |
                AST_FUN of string * term | AST_APP of term * term|
                AST_LET of (string * term * term)| AST_ERROR of string |
                AST_REC of (string * term);


use "parser.sml";

datatype result = RES_ERROR of string | RES_NUM of int| RES_BOOL of bool |
                  RES_SUCC | RES_PRED | RES_ISZERO |
                  RES_FUN of (string * term)

datatype env    = Env of (string -> result)

exception  Error of string
exception  UnboundID of string
exception  Not_implemented

fun emptyenvFun  (x : string) : result = raise UnboundID x

val emptyenv = Env emptyenvFun

fun look_up (Env e) x = e x

(* e [x=v]  update e x v *)

(*  update : env -> string -> result -> string -> result  *)
fun update env (x : string) (value : result) =
                  fn y => if x = y then value else look_up env y


fun interp  (env, AST_NUM n)         = RES_NUM n
|   interp  (env, AST_BOOL b)        = RES_BOOL b
|   interp  (env, AST_ID x  )        = look_up env x
|   interp  (env, AST_SUCC)          = RES_SUCC
|   interp  (env, AST_PRED)          = RES_PRED
|   interp  (env, AST_ISZERO)        = RES_ISZERO
|   interp  (env, AST_ADD)           = raise (Error "not enough arguments for +")
   (* + e1 e2 *)
|   interp  (env, AST_APP(AST_APP(AST_ADD,e1),e2)) =  
             (case interp (env, e1) of
                RES_NUM n1 => (case interp (env, e2) of
                                   RES_NUM n2 => RES_NUM (n1+n2)
                                  | _         => raise (Error "not a number"))
              | _          => raise (Error "not a number")
                       )
   (* + e1 *)
|  interp (env, AST_APP(AST_ADD,e1)) = raise (Error "not enough arguments for +") 
   (* if e1 then e2 else e3 *)
|  interp  (env, AST_IF (e1,e2,e3)) = (case interp (env,e1) of
                            RES_BOOL true  => interp  (env,e2)
                          | RES_BOOL false => interp  (env,e3)
                          | _              => raise
                                             (Error "if condition non-bool!") )
    (* fn x => body *)
|   interp  (env, AST_FUN(x,body))   = RES_FUN (x,body)
    (* let x = e1 in e2 end   *)
|   interp  (env, AST_LET (x,e1,e2)) = let val v1 = interp (env, e1)
                                        in interp (Env (update env x v1), e2)
                                       end
    (* e1 e2 -- (fn x => x )(+ 2 2) *)
| interp   (env, AST_APP (e1,e2))   =
       (case interp  (env, e1) of
         RES_FUN (formal, body) => let val actual  = interp (env, e2)
                                       val new_env = Env (update env formal actual)
                                   in interp (new_env, body)
                                   end
       | RES_SUCC =>
         (case interp (env,e2) of
              RES_NUM n => RES_NUM (n+1)
            | _         => raise (Error "succ non-number"))
       | RES_PRED =>
         (case interp (env,e2) of
              RES_NUM 0 => RES_NUM 0 
            | RES_NUM n => RES_NUM (n-1)
            | _         => raise (Error "pred non-number"))
       | RES_ISZERO =>
         (case interp (env,e2) of
              RES_NUM n => RES_BOOL (n = 0)
            | _         => raise (Error "iszero non-number"))
       | _ => raise (Error "apply non-function") )
 |  interp (env, AST_ERROR s)       = raise (Error s)
 |  interp (env, _)                 = raise Not_implemented;



fun interp_top s = interp (emptyenv, parsestr s)
                     handle (Error z)     =>  RES_ERROR z
                    |       (UnboundID x) => RES_ERROR("Variable Unbound " ^ x );


(*   Example A - static: 2 -  dynamic : 100 *)
val test_A = "let x = 2 in \
                    \let f = fn z => x  in \
                      \ let  x = 100 in \
                        \ (f x) end end end"
val res_A = interp_top test_A

(*   Example  B - static : 17  dynamic:  error *)

val test_B = "let x = 1 in \
              \let f = fn y => let  w = + y 1 \
                \                in fn z => + (+ (+ x y) z) w end \
                  \in let x = 8 \
                    \ in let g = f 6 \
                      \   in let y = 5 \
                        \    in g 3 \
                        \ end end end end end"
val res_B = interp_top test_B

(*   Example  C - static : 6 - dynamic: 5 *)

val test_C = "let f  = fn  g => let x = 3 in g 2 end \
               \in  let x = 4  \
                \    in let h = fn y => + x y   \
                 \      in f h \
                   \    end end end"


val res_C = interp_top test_C

(*   Example  D  static : 14 - dynamic : 17 *)
val test_D = "let  x = 4 in \
               \ let f = fn y => + x y in \
                \ let g = fn h => let x = 7 in + (h 3) x end in \
                 \ g f end end end"
val res_D = interp_top test_D



(*   Example  E - static : 9 - dynamic : 21  *)

val test_E ="let f1 = fn y=> let x = + y 1 in fn z => let t1 = + x y \
\                                                      in + t1  z \
\                                                      end \
\                            end \
\             in let y = 0  \
\                 in let x = 17 \
\                     in let q = 0 \
\                         in let r1 = f1 2  \
\                            in  r1 4 \
\                            end \
\                         end \
\                    end \
\                 end \
\             end"


val res_E = interp_top test_E;

   (*   Examples  F  and G 
         static : 19 and 19  dynamic: 5 and 19 *)

val test_F = "let f3 = fn g => let x = 3 in g 2 end \
\             in let x = 17   \
\                in f3 (fn y => + x  y)  \
\                end \
\            end " 

val test_G = "let f4 = fn g => g 2  \
\            in let x = 17  \
\                in f4 (fn y => + x y) \
\                end \
\            end"

val res_F = interp_top test_F
val res_G = interp_top test_G

(*****************************************)



