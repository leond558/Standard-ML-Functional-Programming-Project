fun rootplus (a, b, c) :real = ( ~b + Math.sqrt (b*b-4.0*a*c)) / (2.0*a) :real;
fun evalquad (a, b, c, x) :real = (a*x*x) + (b*x) + c :real;

fun facr(n) : int =
	if n<=1
	then 1
	else n * facr(n-1);
	
fun tailfaci(x, y) =
    if x>1
    then tailfaci(x-1, y*x)
    else y;

	
fun faci(n) =
    if n>1
    then tailfaci(n,1)
    else 1;

	
fun sumgeneralise(x:real, n:int) :real=
	if n = 0
	then 0.0
	else x + sumgeneralise(x/2.0, n-1);
	
fun sumt(n:int) =
	sumgeneralise(1.0,n);
	