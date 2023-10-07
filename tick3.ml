exception empty;
datatype 'a tree = Lf
					| Br of 'a * 'a tree * 'a tree

fun arrayoflist [] = raise empty
	|arrayoflist(xs)= 
		let fun tcons v Lf = Br(v,Lf,Lf)
				|tcons v (Br(w,t1,t2)) = Br(v, tcons w t2, t1)
(* The tcons is a curried function that takes two arguments, the value
that is to be incorporated into the functional array and the current 
state of the array. tcons takes this value and adds it as the root of the 
binary tree, switches the subtree branches and then iterates the process for
the removed value with the right subtree for what is now the new left subtree.
This ensures that the functional array is produced in accordance to requirements
but has the consequence of producing a reversed array relative to the order of the list.*)
			fun arrayhelper ([], arraylist) = arraylist
				|arrayhelper (l::ls,arraylist) = 
					arrayhelper(ls, tcons l arraylist)
		in arrayhelper(rev(xs),Lf)
		end;

fun listofarray Lf = []
	|listofarray (Br(w,t1,t2)) = 
		let fun oneremove Lf = Lf
				|oneremove (Br(a, Lf, Lf)) = Lf
				|oneremove (Br(a, t1, t2)) = 
			let fun extractor (Br(a, t1, t2)) = a
			in Br(extractor(t1),t2,oneremove(t1))
			end
(* This function removes the first element from the functional array and 
then proceeds to reconstruct it preserving subscript order. This enables 
construction of a list from the functional array through purely consing
the first element of the working array to the resultant list and then
removing this root nodal element from the array.*)
		in w::listofarray(oneremove(Br(w,t1,t2)))
		end;
		
fun getSubsOfEvens Lf  = []
	|getSubsOfEvens arr =
	let fun evensubextract ([],ls,_) = rev(ls)
			|evensubextract (x::xs,ls,n) = 
				if x mod 2 = 0
				then evensubextract(xs,n::ls,n+1)
				else evensubextract(xs,ls,n+1)
	in evensubextract(listofarray(arr),[],1)
	end;