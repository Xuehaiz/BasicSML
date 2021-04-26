(*  Concrete syntax

e :: x | n | true | false | succ | pred | iszero | add 
   | if e then e else e
   | fn x => e | e e | (e) | let x = e in e end

*)

datatype term = AST_ID of string | AST_NUM of int  | AST_BOOL of bool|
                AST_SUCC | AST_PRED | AST_ISZERO | AST_ADD |
                AST_IF of term * term * term |
                AST_FUN of string * term | AST_APP of term * term|
                AST_LET of (string * term * term)| AST_ERROR of string |
                AST_REC of (string * term);


use "parser.sml";

val test_string_A = "2"
val test_term_A = parsestr test_string_A 

val test_string_B = true
val test_term_B = parsestr test_string_A

val test_string_C = "+ 2 3"  
val test_term_C = parsestr test_string_C

val test_string_D = "+ 2"     
val test_term_D = parsestr test_string_C

val test_string_E = "fn x = 2 "  
val test_term_E = parsestr test_string_D   

val test_term_F = parsestr "fn = + 2 3 "    

val test_term_G = parsestr "let x = 2 in 3  " 

val test_term_H = parsestr "let x = 2 3 end"   

val test_term_I = parsestr "x y z  "  

val test_term_L  = parsestr "x (y z)  " 


val test_string_M = "let x = 2 in \
                      \let f = fn z => x  in \
                       \ let  x = 100 in \
                        \ (f x) end end end";


val test_term_M = parsestr  test_string_A  

 