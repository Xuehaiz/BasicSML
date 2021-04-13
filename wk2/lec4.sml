datatype grade = A | B | C | D | F 

val temp = A


fun policy n = if       90 <= n andalso n <= 100 then A
	              (* <= stands for less or equal and
	                 >= for greater or equal *)
	            else if 80 <= n andalso n <= 89  then B
	            else if 70 <= n andalso n <= 79  then C
	            else if 60 <= n andalso n <= 69  then D
	            else F 

fun upgrade_bad lg = if lg = B then A
	                  else if lg = C then B
	                 	else if lg = D then C
	                 		else if lg = F then D
	                 			else lg
	              
fun upgrade g = case g of
	                B => A
	              | C => B
	              | D => C
	              | x => x  (* g: grade -> garde *) 
	              (* the pattern x will match the case of g being
	                 either A or F. However, notice that g cannot be
	                 a P since P is not a value of datatype grade *) 

val test_upgrade  = upgrade A;
val test_upgrade1 = upgrade F;

(* upgrade 1 does not type check *)

(* patterns *) 

fun upgrade' B = A
|   upgrade' C = B
|   upgrade' D = C
|   upgrade' F = D
|   upgrade' x = x 


datatype shape = Square of real
               | Rectangle of  real * real
               (*| Rectangle1 of real * int *)
               | Circle of real

val a = Square 2.3
val b = Rectangle (1.2,3.4)

val ((x,y),z) = ((2,3),5)


fun area s  =
  case s of
      Square l => l * l
    (* | Rectangle z=> #1 z * #2 z *)
    | Rectangle (x,y) => x * y
    | Circle r => 3.14 * r * r

fun area' (Square l) = l * l
  | area' (Rectangle (x, y)) = x * y
  | area' (Circle r) = 3.14 * r * r

val zz = area' b

val (x,y) = (2,3)

fun sum_triple tr =
	 let val (x,y,z) = tr
	 in x+y+z
	 end

