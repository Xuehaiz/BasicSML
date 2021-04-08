datatype json =
         Num of real
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list


fun make_silly_json (i : int) =
	let
		fun helper j =
			case j of
				0 => []
			  |	1 => Object([("n", Num(1.0)), ("b", True)]) :: []
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


fun dot(j, f) =
	case j of
		Object jj => assoc(f, jj)
	  | _ => NONE