exception empty;
fun last [] = raise empty
	| last [x] = x
	| last (x::xs) = last(xs);
	
fun butlast [] = raise empty
	|butlast [x] = []
	|butlast(x::xs) = x::butlast(xs);
	
fun nth ([],_) = raise empty
	|nth (x::xs,0) = x
	|nth (x::xs,n) = nth(xs,n-1);
	
	