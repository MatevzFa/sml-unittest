
fun length(xs) =
    if null xs then 0
    else 1 + length(tl xs);


fun concatenate(xs, ys) = 
	if null(xs) then ys
	else hd xs :: concatenate(tl xs, ys);
