
fun is_older(ft: int*int*int, st: int*int*int) =
    ((#1 ft)*365 + (#2 ft)*30 + #3 ft) < ((#1 st)*365 + (#2 st)*30 + #3 st)

fun number_in_month(dates: (int*int*int) list, month: int) = 
    if null dates
    then 0
    else
	if (#2 (hd dates)) = month
	then 1 + number_in_month(tl dates,month)
	else number_in_month(tl dates,month)

fun number_in_months(dates: (int*int*int) list, months: int list) = 
    if null months
    then 0 
    else number_in_month(dates,hd months) + number_in_months(dates,tl months)

fun dates_in_month(dates: (int*int*int) list, month: int) = 
    if null dates
    then []
    else 
	if #2 (hd dates) = month
	then dates_in_month(tl dates,month) @ [hd dates]
	else dates_in_month(tl dates,month)

fun dates_in_months(dates: (int*int*int) list, months: int list) = 
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings: string list, n: int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n-1)

fun date_to_string(date: int*int*int ) = 
    let
	val months = ["January", "February", "March","April","May","June","July","August","September","October","November","December"];
    in
	get_nth(months,#2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum: int, numbers: int list) = 
    let 
	fun suma(numbers: int list,n: int,sum': int) = 
	    if (sum' + hd numbers) >= sum andalso (sum' < sum)
	    then n
	    else suma(tl numbers,n+1,sum'+hd numbers)
    in
	suma(numbers,0,0)
    end


fun what_month(day: int) = 
    number_before_reaching_sum(day,[31,28,31,30,31,30,31,31,30,31,30,31])+1

fun month_range(day1:int, day2:int) = 
    if day1>day2
    then []
    else 
	let
	    fun make_list(day: int)=
		if day <= day2
		then what_month(day) :: make_list(day+1)
		else []
	in 
	    make_list(day1)
	end

fun oldest(dates: (int*int*int) list) = 
    if null dates
    then NONE
    else
	let val old = oldest(tl dates)
	in if isSome old andalso is_older(valOf old,  hd dates)
	   then old
	   else SOME(hd dates)
	end 
