(* HW3 Xuehai Zhou*)

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
val caps_no_X_string = (String.map Char.toUpper) o List.filter(fn c => Char.toUpper(c) <> #"X") o String.explode




