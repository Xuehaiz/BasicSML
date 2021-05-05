fun interp_static  (env, AST_SUCC)          = RES_SUCC
|   interp_static  (env, AST_PRED)          = RES_PRED
|   interp_static  (env, AST_ISZERO)        = RES_ISZERO
|   interp_static  (env, AST_NUM n)         = RES_NUM n
|   interp_static  (env, AST_BOOL b)        = RES_BOOL b
|   interp_static  (env, AST_ID x  )        = look_up env x 
|   interp_static  (env, AST_IF (e1,e2,e3)) = (case interp_static(env, e1) of
                                          RES_BOOL true  => interp_static(env, e2)
                                        | RES_BOOL false => interp_static(env,e3)
                                        | _              => raise Error "Non Bool in IF")

|   interp_static  (env, AST_FUN(x,body))   = RES_FUN(x, body)
|   interp_static  (env, AST_LET (x,e1,e2)) = let val v1 = interp_static (env, e1)
                                        in interp_static (Env (update env x v1), e2)
                                       end

   
|  interp_static  (env, AST_APP(AST_APP(AST_ADD,e1),e2)) =  (* + e1 e2 *)
             (case interp_static (env, e1) of
                RES_NUM n1 => (case interp_static (env, e2) of
                                   RES_NUM n2 => RES_NUM (n1+n2)
                                  | _         => raise (Error "not a number"))
              | _          => raise (Error "not a number") 
                       )
| interp_static (env, AST_APP(AST_ADD,e1)) = raise (Error "not enough arguments for +") (* + e1 *)
| interp_static   (env, AST_APP (e1,e2))   = (case ((interp_static(env, e1), interp_static(env, e2))) of
                                        (RES_SUCC, RES_NUM n)   => RES_NUM(n+1)
                                      | (RES_PRED, RES_NUM n)   => RES_NUM(n-1)
                                      | (RES_FUN(f, p), a)   => interp_static(Env (env update f a), p)
                                      | (RES_ISZERO, RES_NUM n)   => RES_NUM(n=0)
                                      | _ => raise Error "Incorrect term in function application") 
 |  interp_static (env, AST_ERROR s)       = raise (Error s)
 |  interp_static (env, _)                = raise Not_implemented;