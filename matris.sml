
(*
additions och subtraktionsfunktioner som du kallar genom funktionen operation
*)

fun add' ([], []) = []
  | add' ((x::xs), (y::ys)) = x + y :: add' (xs, ys)


fun sub' ([], []) = []
  | sub' ((x::xs), (y::ys)) = x - y :: sub' (xs, ys)
			     


fun operation (f, [], []) = [] 
  | operation (f, x, []) = x
  | operation (f, (x::xs), (y::ys)) = f (x, y) :: operation (f, xs, ys)



fun add (x, y) = operation (add', x, y)


fun sub (x, y) = operation (sub', x, y)



fun listAdd [] = []
  | listAdd (x::xs) = add(x, listAdd xs)


fun listSub [] = []
  | listSub (x::xs) = sub(x, listSub xs)





(*
--------------------------------------------------------

funktionen flipp som flippar runt listan innan multiplikationen
*)


fun delete [] = []
  | delete (x::xs) = tl x :: delete xs



fun flipp' [] = []
  | flipp' (x::xs) = hd x :: flipp' xs


fun flipp [] = [] 
  | flipp x = if hd x = [] then 
		   [] else 
	       flipp' x :: flipp (delete x)
(*
---------------------------------------------------------
*)




val bajs = [[1,2],[1,2],[1,2],[1,2],[1,2]]
val bajs2 = [[2], [2], [2], [2], [2]]












(*
---------------------------------------------------------------------------------
multiplikationsfunktionerna. huvudfunktionen heter multiply
*)






fun movePos (x, y) = List.drop (x, y) @ List.take (x, y) 

fun moveNeg (x, y) = List.drop (x, length x - y) @ List.take (x, length x - y)



fun move (f, [], _) = []
| move (f, (x::xs), y) = f (x, y) :: move(f, xs, y + 1)

fun move' (f, x) = move (f, x, 0)






fun line (x, y) = List.take (x, y - 1) @ List.drop (x, y)









fun mult' [] = 1
  | mult' (x::xs) = x * mult' xs 

fun mult [] = 0 
  | mult (x::xs) = mult' x + mult xs















fun listMult [] = []
  | listMult (x::xs) = 
    let
	fun multi' ([], []) = 0 
	  | multi' ((x::xs), (y::ys)) = x * y + multi' (xs, ys)

	fun multi (_,[]) = [] 
	  | multi (x,(y::ys)) = multi' (x,y) :: multi (x, ys)

	fun multiply' ([], _) = [] 
	  | multiply' ((x::xs), (y::ys)) = multi (x, y::ys) :: multiply' (xs, (y::ys))

	fun multiply (x,[]) = x
	  | multiply (x,y) = multiply' (x, flipp y)
    in
	multiply (x, listMult xs)
    end 






















val kkk = [[5,5,5],[4,0,0],[1,0,1]]







fun determinant [[x]] = x 
  | determinant matrix = let val ((y::ys::yss)::(x::xs::xss)::zs) = matrix in 
			     if length matrix = 2 then
				 y * xs - ys * x
			     else
				 mult (flipp (move'(movePos, flipp matrix))) - mult (flipp (move'(moveNeg, flipp matrix)))

			 end




fun invers' ([], y) = [] 
  | invers' (x, y) = if length (flipp x) < y then 
			[]
		    else
			determinant (line ( flipp x, y)) :: invers'(x, y + 1) 


fun invers ([],_) = []
  | invers (x, y) = if length x < y then 
			 []
		     else 
			 invers' (line ( x, y), 1) :: invers (x, y + 1)












val h = [[1,2],[3,5]]





val mar = [[1,0,1],[5,4,9],[3,7,0]]

val kar = [[4,9],[7,0]]


val new = [[1,2,3],[3,4,0],[1,1,1]]







val test = [[1,2,3],[0,4,5],[1,0,6]]


fun minus'' ([], _) = [] 
  | minus'' ((x::xs), y) = x * y * ~1 :: minus''(xs, y * ~1) 

fun minus' ([], _) = [] 
  | minus' ((x::xs), y) = minus'' (x, y) :: minus' (xs, y * ~1) 

fun minus matrix = minus' (matrix, ~1)


fun inv x = minus (invers (x, 1))

val test1 = [[1,2,3,4],[5,6,7,0],[9,0,11,12],[13,14,15,0]]








fun det' ([], y) = [] 
  | det' (x, y) = if length (flipp x) < y then 
			[]
		    else
			flipp (line ( flipp x, y)) :: det'(x, y + 1) 


fun det ([],_) = []
  | det (x, y) = if length x < y then 
			 []
		     else 
			 det' (line ( x, y), 1) :: det (x, y + 1)




(*
fun forsta ((x::xs), y) = hd x * flipp line ((flipp (line ((x::xs), 1))), 1)

fun forsta ((x::xs), y) = let val ((y::ys::yss)::(z::zs::zss)::ks) = (x::xs) in 
			      if length x = 2 then
				  y * zs - ys * z
			      else if length x < y then
				  0
			      else
				  List.nth(x, y) * forsta (line (flipp (line ((x::xs), y)), 1), 1) 
			  end

*)





val mat = [[1,2,3],[4,5,6],[7,8,9]]


val matris = [[1,2],[5,6],[1,2],[1,2]]
val matris1 = [[1,2,3,4],[7,8,8,12]]

val h = [[1, 7], [2, 8]]

val kk = ([1,2,3,4],[1,3,5,7])

