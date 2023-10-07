fun nfold f 1 = (fn x => f x)
(*if the function need only be applied once more, apply the function to the
supplied argument*)
	|nfold f n = (fn x => nfold f (n-1) (f x));
(*if the function needs applying for n occurences, as nfold is a functional
that outputs a function that in itself can be provided with an argument,
successive application of the desired function can be achieved through recursively
calling nfold with a decremented count and with the function applied to the 
argument as the next argument*)
	
fun sum n m = nfold (fn x => x+1) n m;
fun product n m = nfold (fn x => x+m) n 0;
(*add m to the provided input for n number of occurences, must be initialised
with a 0 value*)
fun power n m = nfold (fn x=> product x n) m 1;
(*n raised to the power m can be found through successive multiplication
of n by m times, again this must be initalised at 1*)

datatype 'a stream = Cons of 'a * (unit -> 'a stream);
fun head (Cons(x,_)) = x;
(*extract the head element from the sequence*)
fun tail (Cons(_,xf)) = xf();
(*evaluate the function containing the next elements of the sequence to give
the remaining terms excluding the head*)
fun nth(s,1) = head s
(*if at the position desired, extract the value*)
	|nth(s,n) = nth((tail(s),n-1));
(*evaluate the remaining elements in the sequence and decrement the position counter*)
fun squares k = Cons((k*k), fn()=> squares(k+1));
(*value at a position is equal to the square of its position number, increment
the position number for successive elements*)

fun map2 f xs ys = Cons(f (head(xs)) (head(ys)), fn()=> map2 f (tail(xs)) (tail(ys)));
(*given a function and two sequences, generate a sequence with the element being
the function applied to the head elements of both input sequences and let the
remaining sequences be the the function called again with the tails of the
sequences*)
	