(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare                                        
                        
(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

(* We now load a file with police data represented as values of type json:
   small_incident_reports (10 reports).
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";




(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;



(**** PUT PROBLEMS 1-8 HERE ****)

fun make_silly_json (i : int) =
  let
      fun helper j =
        case j of
          0 => []
          | _ => Object([("n", Num(Real.fromInt(j))), ("b", True)]) :: helper(j - 1)
  in
      Array(helper(i))
  end


fun assoc(k, xs) = 
  case xs of
    [] => NONE
    | (k1, v1) :: xs' => if k1 = k
                         then SOME v1
                         else assoc(k, xs')


fun dot(j : json, f : string) =
  case j of
    Object jj => assoc(f, jj)
    | _ => NONE


fun one_fields (j: json) =
  let fun fieldsNames(xs, names)= 
        case xs of
          [] => names
        | (str, js)::xs' => fieldsNames(xs', str::names) 
  in
      case j of
        Object j => fieldsNames(j, [])
        | _ => []
  end


fun no_repeats (strList : string list) =
  length (dedup strList) = length strList


fun recursive_no_field_repeats (js : json) =
  let
      fun arrayTravesal js =
        case js of
          [] => true
          | j::js' => (recursive_no_field_repeats j) andalso (arrayTravesal js')

      fun objectTravesal js =
        case js of
          [] => true
          | (str, j)::js' =>  (recursive_no_field_repeats j) andalso (objectTravesal js')     
  in
      case js of
        Array a => arrayTravesal(a)
        | Object b => no_repeats (one_fields js) andalso objectTravesal b
        | _ => true
  end


fun count_occurrences (li, err) =
  let 
      fun count (l, current_string, current_count, countList) = 
        case l of
          [] => (current_string, current_count) :: countList
          | xs::l' => case strcmp(xs, current_string) of
                         LESS => count (l', xs, 0, (current_string, current_count)::countList)
                         | EQUAL => count (l', xs, current_count + 1, countList)
                         | GREATER => raise err
  in
      case li of
        [] => []
        | xs::li' => count(li', xs, 1, [])
  end


fun string_values_for_field (str, arr) =
  case arr of
    [] => []
    | a::arr' => case dot(a, str) of
                   SOME (String s) => s::string_values_for_field(str, arr')
                   | _ => string_values_for_field(str, arr')


(* histogram and historgram_for_field are provided, but they use your 
   count_occurrences and string_values_for_field, so uncomment them 
   after doing earlier problems *)

(* histogram_for_field takes a field name f and a list of objects js and 
   returns counts for how often a string is the contents of f in js. *)

exception SortIsBroken

fun histogram (xs : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

fun histogram_for_field (f,js) =
  histogram (string_values_for_field (f, js))


(**** PUT PROBLEMS 9-11 HERE ****)

;Control.Print.printDepth := 3;
Control.Print.printLength := 3;

fun filter_field_value (str1, str2, arr) = 
  case arr of
    [] => []
    | a::arr' => case dot(a, str1) of
                   SOME (String s2) => if str2 = s2
                                       then a::filter_field_value(str1, str2, arr')
                                       else filter_field_value(str1, str2, arr')
                   | _ => filter_field_value(str1, str2, arr')


val large_event_clearance_description_histogram = histogram_for_field("event_clearance_description", [small_incident_reports]);

val large_hundred_block_location_histogram = histogram_for_field("hundred_block_location", [small_incident_reports]);

(**** PUT PROBLEMS 12-15 HERE ****)

;Control.Print.printDepth := 20;
Control.Print.printLength := 20;

val forty_third_and_the_ave_reports = filter_field_value("hundred_block_location", "43XX BLOCK OF UNIVERSITY WAY NE", [small_incident_reports]);

val forty_third_and_the_ave_event_clearance_description_histogram = histogram_for_field("event_clearance_description", forty_third_and_the_ave_reports);

val nineteenth_and_forty_fifth_reports = filter_field_value("hundred_block_location", "45XX BLOCK OF 19TH AVE NE", [small_incident_reports]);

val nineteenth_and_forty_fifth_event_clearance_description_histogram = histogram_for_field("event_clearance_description", nineteenth_and_forty_fifth_reports);


(**** PUT PROBLEMS 16-19 HERE ****)

fun concat_with(sep : string, strlist : string list) =
  if strlist 




