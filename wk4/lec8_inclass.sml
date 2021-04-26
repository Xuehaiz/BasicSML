Control.printWarnings := false;

fun sum (x,y) = x+y;

(* curried version of sum *)
fun plus x y = x + x ;	
fun plus1 x = fn y => x + y ;
	
(* int -> (int -> int) *)	

(* if e1 evaluates to v1 and e2 evaluate to v2 then e1+e2 evaluate to
    v1+v2

    |- e1 --> v1  |- e2 --> v2
    ----------------------------
    |- e1+e2 --> v1*v2 

    |- e1 --> v1 |- e2 --> v2 v1 >= v2 
    -------------------------------
    |- e1 < e2 --> false  

    |- e1 --> v1 |- e2 --> v2 v1 < v2 
    -------------------------------
    |- e1 < e2 --> true  
*)
(*  exp ::= number | exp + exp | e1 < e2 |
     if e1 then e2 else e3 | fn x => e *)

datatype term = AST_NUM of int | AST_ADD of term * term
              | AST_LESS of term * term 
              | AST_IF of term * term * term 
              | AST_FUN of string * term 
              | AST_APP of term * term 
              | AST_ID of string 

datatype Result = RES_NUM of int | RES_BOOL of bool
                 | RES_FUN of string * term 

datatype env    = Env of (string -> Result)
exception UnboundID of string 

fun emptyenvFun  (x : string) : Result = raise UnboundID x;

val emptyenv = Env emptyenvFun

fun look_up (Env e) x = e x

(* e [x=v]  update e x v *)

(*  update : env -> string -> result -> string -> result  *)
fun update env (x : string) (value : Result) =
                  fn y => if x = y then value else look_up env y
                

(*  2+(3+4) *)
val ex = AST_ADD (AST_NUM 2, AST_ADD(AST_NUM 3, AST_NUM 4))

(* term -> Result *)
fun interp env e = case e of
	            AST_ADD (e1, e2) => 
	                let val (RES_NUM v1) = interp env e1
	            	    val (RES_NUM v2) = interp env e2
	            	 in RES_NUM (v1*v2) end
	         | AST_NUM x         => RES_NUM x
	         | AST_LESS (e1, e2) => 
	                     let val (RES_NUM v1)  = interp env e1
		                     val (RES_NUM v2)  = interp env e2
		                  in RES_BOOL (v1 < v2)
		                  end 
		    |  AST_IF (e1,e2,e3)  => 
		           let val (RES_BOOL b) = (interp env e1)
                    in if b then (interp env e2) else 
                    	     (interp env e3)	
                    end
          | AST_FUN (x,e)       =>  RES_FUN (x,e)   
          | AST_APP (e0,e1)     => (* e0 e1 *)
                let val (RES_FUN (x,e)) = interp env e0
	                val v1 = interp env e1
	             in interp env e 
	                      end
	      | AST_ID  x    =>  look_up env x                

                	                   
val test_add = interp emptyenv ex;	
val test_add1 = interp emptyenv (AST_NUM (2+(3+4)))
	                      
(* (fn x =>99) 59 
    (fn x => x) 59
    *)	                      
val test_interp = interp emptyenv (AST_APP(AST_FUN("x",AST_NUM 99),AST_NUM 59))
(* 
val test_interp1 = interp emptyenv  (AST_APP(AST_FUN("x", AST_ID "x"),AST_NUM 59))
*)
