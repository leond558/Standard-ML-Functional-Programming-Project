type color = int*int*int   (* RGB colour components, 0..255 *)
type xy = int*int       (* points (x, y) and sizes (w, h) *)
datatype image = Image of xy * color array array;

fun format4 i = StringCvt.padLeft #" " 4 (Int.toString i);

fun image ((w,h):xy) ((r,g,b):color) = 
					Image( (w,h), Array.tabulate(h, fn i => Array.tabulate(w , fn j => (r,g,b) :color)));
								

fun size (Image(dim,_))= dim;

fun drawPixel (Image((w,h),colar)) (cr) ((x,y):xy) = 
	let val rowar = Array.sub(colar,y)
	in Array.update(rowar,x,cr)
	end;
	
fun toPPM (Image(dim,colar)) filename = 
	let val oc = TextIO.openOut filename
	    val (w,h) = dim
	in TextIO.output(oc,"P3\n" ^ Int.toString (w) ^ " " ^ Int.toString (h) ^ "\n255\n");
		 Array.app (fn x => ((Array.app (fn (r,g,b) => TextIO.output(oc, format4 r ^ format4 g  ^ format4 b)) (x)); TextIO.output(oc,"\n"))) (colar);
		TextIO.closeOut oc
	end;
								
fun drawHoriz _ _ _ 0 = ()
|	drawHoriz im cr (x, y) len = (drawPixel im cr (x, y); 
								  drawHoriz im cr (x + 1, y) (len - 1));
								  
fun drawVert _ _ _ 0 = ()
|	drawVert im cr (x, y) len = (drawPixel im cr (x, y); 
								  drawVert im cr (x, y+1) (len - 1));
								  
fun drawDiag _ _ _ 0 = ()
|	drawDiag im cr (x, y) len = (drawPixel im cr (x, y); 
								  drawDiag im cr (x + 1, y +1) (len - 1));

fun drawLine im cr ((x0,y0):xy) ((x1,y1):xy) =
	let val dx = Int.abs(x1-x0)
		val dy = Int.abs(y1-y0)
		val sy = Int.sign(y1-y0)
		val sx = Int.sign(x1-x0)
		fun drawHelper x y erra = (
			drawPixel im cr (x,y);
			if (x=x1) andalso (y=y1)
			then ()
			else if ((2*erra)> ~dy) andalso ((2*erra)<dx)
				then drawHelper (x+sx) (y+sy) (erra-dy+dx)
				else if ((2*erra)> ~dy)
					then drawHelper (x + sx) y (erra-dy)
					else if ((2*erra)<dx) 
						then drawHelper x (y+sx) (erra+dx)
						else ()
						)
	in drawHelper x0 y0 (dx-dy)
	end;

	
		
		
		
		
