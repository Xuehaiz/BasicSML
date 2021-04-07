(* HW1 Xuehai Zhou *)

fun is_older(day1 : int * int * int, day2 : int * int * int) = 
	if (#3 day1) < (#3 day2) 
	then true
	else
		if (#2 day1) < (#2 day2) andalso (#3 day1) = (#3 day2)
		then true
		else
			if (#1 day1) < (#1 day1) andalso (#2 day1) = (#2 day2)
			then true
			else false


fun number_in_month(dates : (int * int * int) list, month : int) = 
	if null dates
	then 0
	else if #2 (hd dates) = month
	then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month)


fun number_in_months(dates : (int * int * int) list, months : int list) =
	if null dates
	then 0
	else if null months
	then 0
	else number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month(dates : (int * int * int) list, month : int) = 
	if null dates
	then []
	else if #2 (hd dates) = month
	then (hd dates) :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)


fun dates_in_months(dates : (int * int * int) list, months : int list) = 
	if null dates
	then []
	else if null months
	then []
	else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


fun get_nth(strings : string list, n : int) = 
	if n = 1
	then (hd strings)
	else get_nth(tl strings, n - 1)


val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
fun date_to_string(date : int * int * int) = 
	get_nth(months, #2 date)^"-"^Int.toString(#1 date)^"-"^Int.toString(#3 date)


fun number_before_reaching_sum(sum : int, l : int list) = 
	let
		fun iter_sum(i : int, stop_sum : int, max : int, li : int list) =
			if stop_sum + (hd li) >= max
			then i - 1
			else iter_sum(i + 1, stop_sum + (hd li), max, tl li)
	in
		iter_sum(1, 0, sum, l)
	end
	

val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
fun what_month(day : int) = 
	number_before_reaching_sum(day, month_days) + 1


fun month_range(day1 : int, day2 : int) =
	if day1 > day2
	then []
	else 
		let
			fun iter_append(month1 : int, month2: int) =
				if month1 > month2
				then []
				else month1 :: iter_append(month1 + 1, month2)
		in
			iter_append(what_month(day1), what_month(day2))
		end


fun oldest(dates : (int * int * int) list) = 
	if null dates
	then NONE
	else
		let
			fun get_oldest(dates_l : (int * int * int) list) =
				if null (tl dates_l)
				then (hd dates_l)
				else
					let 
						val first = hd dates_l
						val last = get_oldest(tl dates_l)
					in
						if is_older(first, last)
						then first
						else last
					end
		in
			SOME (get_oldest(dates))
		end


fun helper(sum : int, numbers : int list) =
	if null numbers
	then []
	else (sum + (hd numbers)) :: helper(sum + hd numbers, tl numbers)


fun cumulative_sum(numbers : int list) =
	helper(0,numbers);








