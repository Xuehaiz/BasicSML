fun is_older(day1 : int*int*int, day2 : int*int*int) = 
	if (#3 day1) < (#3 day2) then true
	else if （#2 day1） < (#2 day2) then true
	else if (#1 day1) < (#1 day2) then true
	else false