(* HW3 Xuehai Zhou *)

(* 1 *)
fun only_lowercase (l : string list) = List.filter (fn s => Char.isLower(String.sub(s, 0))) l;

only_lowercase ["HELLO", "WORLD", "hello", "world", "beautiful"];


(* 2 *)
fun longest_string1 (l : string list) = foldl (fn (s, longest) => if String.size(s) > String.size(longest) then s else longest) "" l;

longest_string1 ["HELLO", "WORLD", "hello", "world", "beautiful", "caseoftie"];

(* 3 *)
fun longest_string2 (l : string list) = foldl (fn (s, longest) => if String.size(s) >= String.size(longest) then s else longest) "" l;

longest_string2 ["HELLO", "WORLD", "hello", "world", "beautiful", "caseoftie"];

(* 4 *)
fun longest_string_helper f l = foldl (fn (s, longest) => if f(s, longest) then s else longest) "" l;

val longest_string3 = (fn (s, longest) => String.size(s) > String.size(longest));
val longest_string4 = (fn (s, longest) => String.size(s) >= String.size(longest));

longest_string_helper longest_string3 ["HELLO", "WORLD", "hello", "world", "beautiful", "caseoftie"];
longest_string_helper longest_string4 ["HELLO", "WORLD", "hello", "world", "beautiful", "caseoftie"];

(* 5 *)
val longest_lowercase = (longest_string1 o only_lowercase);
longest_lowercase ["HELLO", "WORLD", "hello", "world", "Beautiful", "beautiful", "caseoftie"];

(* 6 *)
val caps_no_X_string = (String.map Char.toUpper) o String.implode o List.filter(fn c => Char.toUpper(c) <> #"X") o String.explode;

caps_no_X_string "aBxXXxDdx";

(* 7 *)
exception NoAnswer;

fun first_answer f li =
	case li of
		[] => raise NoAnswer
	|	x::li' => case f x of
					NONE => first_answer f li'
				| 	SOME p => p

(* 8 *)
fun all_answers f li = 
	let 
		fun helper(li, acc) = 
			case li of 
				[] => SOME acc
			|	x::li' => case f x of
					NONE => NONE
				|	SOME p => helper(li', p @ acc)
	in 
		helper(li, [])
	end

(* 9 *)
(* given *)
datatype pattern 
	= WildcardP
	| VariableP of string
	| UnitP
	| ConstantP of int
	| ConstructorP of string * pattern
	| TupleP of pattern list

datatype valu 
	= Constant of int
	| Unit
	| Constructor of string * valu
	| Tuple of valu list


fun g f1 f2 p =
    let
        val r = g f1 f2
    in
        case p of
            WildcardP		  => f1 ()
          | VariableP x 	  => f2 x
          | ConstructorP(_,p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
	end

(*
g: count number of patterns matched
wildcard 			-> run f1 with nothing (unit)
variable 			-> run f2 with x
constructor(_, p) 	-> run g again with p, discard _
tuple 				-> pass everything to g, return amount of times folded
_					-> 0
*)

val count_wildcards = g (fn _ => 1) (fn _ => 0);

val count_wild_and_variable_lengths = g (fn _ => 1) (fn n => String.size(n));

val count_a_var (str, p) =
	g (fn _ => 0) (fn n => if n = str then 1 else 0) p;


(* 10 *)
fun check_pat p =
	let fun get_vals p =
			case p if
				VariableP s => [s]
			|	ConstructorP(_,p1) => get_vals p1
			|	TupleP ps => List.foldl (fn (p, vars) => get_vals p @ vars) [] ps
			| 	_ => []
		fun dedup xs = ListMergeSort.uniqueSort String.compare xs
		fun unique xs = (length xs) = length(dedup(xs))
	in
		(unique o get_vals) p
	end;


(* 11 *)
fun match (v, p) = 
	case (v, p) of 
		(_, WildcardP wc) => SOME []
	|	(_, VariableP s) => SOME [(s, v)]
	|	(Unit, UnitP) => SOME []
	|	(Constant i, ConstantP i') => if i = i' then SOME [] else NONE
	|	(Constructor (s1, v), ConstantP (s2, p)) => if s1 = s2 then match (v, p) else NONE
	|	(Tuple vs, TupleP ps) => if length(vs) = length(ps) 
								 then all_answers match(ListPair.zip(vs, ps))
								 else NONE
	|	_ => NONE


(* 12 *)
fun first_match (v, ps) =
	SOME (first_answer(fn pat => match(v, pat)) ps)
	handle NoAnswer => NONE


































