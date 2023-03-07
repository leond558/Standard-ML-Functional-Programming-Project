fun union (xs,ys) =
	let 
		fun member(x,[]) = false
			|member(x, y::l) = (x=y) orelse member(x,l)
		fun avoidrep([],us) = us
			|avoidrep(l::ls,us) =
				if member(l,us) then avoidrep(ls,us)
				else avoidrep(ls, l::us)
	in avoidrep(xs@ys,[])
	end;
	
fun negpart (xs) = 
	let fun dualcons([],ns,ps) = (ns,ps)
			|dualcons(l::ls,ns,ps) =
			if l >= 0 then dualcons(ls,ns,l::ps)
			else dualcons(ls,l::ns,ps)
	in dualcons(xs,[],[])
	end;

fun bubblesort [] =[]
	|bubblesort (xs) = 
	let fun bubble [] = []
			|bubble [x] = xs
			|bubble (x::y::ls) = if x>y then y::bubble(x::ls)
								 else x::bubble(y::ls)
		fun finalised [] = true
			|finalised [x] = true
			|finalised (s1::s2::ss) = s1<s2 andalso finalised(s2::ss) 
	in if finalised (xs) then xs
		else bubblesort (bubble xs)
	end;
	
fun select [] = []
	|select (e::es) = 
	let fun selectsort ([],x,ls) = x::select(ls)
		|selectsort (x2::xs, x1, ls) = 
		if x1 < x2 then selectsort(xs, x1, x2::ls)
		else selectsort(xs, x2, x1::ls)
	in selectsort(es, e, [])
	end;


datatype days = Monday
				| Tuesday
				| Wednesday
				


datatype expression = realnumerical of real
					| variable of string
					| negative of expression
					| summation of expression * expression
					| multiplication of expression * expression;
					
excpetion containsvariable of string;

fun evaluation (realnumerical n) = realnumerical(n) 
	(*if expression trying to be evaluated is of the realnumerical constructor,
	ensure that the real value provided is directly associated with the constructor-
	this is of importance in extraction of this real value as to evaluate the 
	desired arithmetical expression*)
	|evaluation (variable identifier) = raise (containsvariable idefnitfier)
	(*if the expression dataype is of constructor variable then an excpetion is raised*)
	|evaluation (negative n) = 
							let val realnumerical storedvalue = evaluation n
							in realnumerical (~storedvalue)
	(*the utility of the inital pattern match of the function definition is clear
	here as the associated value can be accordingly extracted*)
							end
	|evaluation (summation(x,y)) = let val realnumerical storedvalue1 = evaluation x
									let val realnumerical storedvalue2 = evaluation y
									in realnumerical (storedvalue1+storedvalue2)
									end
	|evaluation (multiplication(x,y)) = let val realnumerical storedvalue1 = evaluation x
									let val realnumerical storedvalue2 = evaluation y
									in realnumerical (storedvalue1*storedvalue2)
									end;
					
exception Collision;

fun insert (Lf , b: string , y) = Br((b,y), Lf, Lf) 
	| insert (Br((a ,x) , t1 , t2 ), b, y) = 
	if b<a
	then Br((a ,x), insert (t1 ,b,y), t2 )
	else if a<b
	then Br((a ,x) , t1 , insert (t2 ,b,y))
	else raise Collision;					
	
exception itemnotpresent of int;

fun delete (Br(a,Lf,Lf), b) = 
		if a = b then Lf
		else raise itemnotpresent b
		(*delete a leaf node*)
	|delete (Br(a,t1,Lf), b) = 
		if a=b then t1
		else raise itemnotpresent b
	|delete (Br(a,Lf,t2), b) = 
		if a=b then t2
		else raise itemnotpresent b
		(*scenario with a singular child enables replacement*)
	|delete (Br(a,t1,t2), b) = 
		if b<a then Br(a,delete(t1,b),t2)
		else if a<b then Br(a,t1,delete(t2,b))
		else let fun successor(Br(s,b1,b2)) = successor(b2)
					|successor(Br(s,Lf,_)) = s
			in Br((successor(t2)), t1, delete(t2, successor(t2)))
			end;
		(*This last segment extracts the minimal, replacing the
		desired item for removal and then recursively calls the delete function 
		on the right subtree from which the minimal was extracted from to 
		prevent item duplication in the binary search tree*)
		
datatype 'a functionalarraybst = Lf
					| Br of 'a * 'a functionalarraybst * 'a functionalarraybst;

fun oneremove Lf = Lf
	|oneremove (Br(a, Lf, Lf)) = Lf
	|oneremove (Br(a, t1, t2)) = 
	let fun extractor (Br(a, Lf, Lf)) = a
			|extractor (Br(a, t1, t2)) = a
	in Br(extractor(t1),t2,oneremove(t1))
	end;
	
fun lexogorder comparex comparey firstpair secondpair = 
	comparex firstpair secondpair orelse comparey firstpair secondpair;
	
fun comparex ((x,y),(a,b)) =
	x<a;

fun comparey ((x,y),(a,b)) =
	x=a andalso y<b;
	
fun map2 f [] = []
	|map2 f (x::xs) = let fun listmap [] = []
							listmap (x::xs) = (f x) :: listmap (f xs)
						in listmap(x::xs)
						end
	|map2 f (ls::lss) = map2 f ls :: map2 f lss;
	
fun ​change​(till,​0​) = [[]] 
	| ​change​ ([],amt) = [] 
	| ​change​ (c::till,amt) =  
		if​ amt < c ​then​ ​change​(till,amt) 
		else let val cons = map (fn(cs) => c::cs)
			in cons(change(C::till,amt-c)) @ change(till,amt);
			end​; 
			
			
	
datatype 'a seq = Nil | Cons of 'a * (unit-> 'a seq);

fun smap f Nil = Nil
	| smap f (Cons(x, xf)) = Cons(f x, fn()=> smap f (xf()));
	
fun exists p [] = false
	| exists p ((a,b)::xs) = (p (a,b)) orelse exists p xs;
	
fun member([(m,n)],us) =
	exists (fn (a,b) => m=a andalso n=b) us;

fun subset ([],_) = true 
(*The empty set is a subset of every set, with the inclusion of itself*)
	|subset(ts,[]) = false
(*If the main ordered list is depleted, then the potential sublist is not
contained within the main list*)
	|subset (((m,a)::ts),((n,b)::us)) = 
		if (m=a andalso n=b) andalso subset(ts,us)
(*If both parameters of the element of the ordered list are identical and this
is recursively held for all subsequent elements, then the first list is indeed
a subset of the second*)
		then true
		else subset(((m,a)::ts),(us));
(*It may be the case that the lists are not initally aligned for comparison due
to differing lengths, and if so, recurse the call with the removal of one element
from the main list*)

fun union ((ts),(us)) =
	let fun avoidrep([],ps) = ps
			|avoidrep(((m,a)::ts),ps) =
				if member([(m,a)],ps:list) then avoidrep(ts,ps)
(*If a term to be added is already present in the accumulation corresponding to
the output of union, then recurse the call with the subsequent element and leave
the accumulator ordered list unaffected*)
				else avoidrep((ts),(m,a)::ps)
	in avoidrep((ts@us),[])
(*Cons the two ordered lists together and transfer elements into an accumulator
union list provided that they do not already exist there.*)
	end;

fun filter p [] = []
	| filter p ((a,b)::xs) =
		if p (a,b) then (a,b) :: filter p xs
		else filter p xs;

fun intersection ((ts),(us)) = 
	filter (fn x => member(x,ts)) us;

fun power (b,0) = 1
	|power(b,e) = 
		let val result = ref b
			val base = ref b
			val exponent = ref e
		in while (!exponent>1) do
			(result := (!result * !base); exponent := (!exponent-1));
			!result
		end;
		
fun identitymatrix (n) = 
	 Array.tabulate(n, (fn x => Array.tabulate(n, fn k=> if k=x then 1 else 0)));
