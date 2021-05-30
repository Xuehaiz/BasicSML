

datatype Exp = Lit of int | Div of Exp*Exp;

val counter = ref 0

fun eval (Lit n)        = n
|   eval (Div (e1, e2)) = 
                         let
                           val v1 = eval e1
                           val v2 = eval e2
                         in    
                           (counter := !counter+1; v1 div v2)
                         end;


val test1 = eval (Div (Lit 2 , Lit 1) );
!counter;
counter := 0;

val test2 = eval (Div( Div(Lit(48),Lit(2)), Div(Lit 16, Lit 4) ));
val count = !counter;


