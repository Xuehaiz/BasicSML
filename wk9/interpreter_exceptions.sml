exception Error

datatype Exp = Lit of int | Div of Exp*Exp;


fun eval (Lit n)        = n
|   eval (Div (e1, e2)) = let
                               val v1 = eval e1
                               val v2 = eval e2
                         in     if v2 = 0 then raise Error
                                  else    v1 div v2
                         end;


eval (Div (Lit 1 , Lit 0 ) )
 handle Error => 0;