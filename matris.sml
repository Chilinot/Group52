
(*
additions och subtraktionsfunktioner som du kallar genom funktionen operation
*)

fun add ([], []) = []
  | add ((x::xs), (y::ys)) = x + y :: add (xs, ys)


fun sub ([], []) = []
  | sub ((x::xs), (y::ys)) = x - y :: sub (xs, ys)
 


fun operation (f, [], []) = [] 
  | operation (f, (x::xs), (y::ys)) = f (x, y) :: operation (f, xs, ys)


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

fun korv ([], []) = 0 
  | korv ((x::xs), (y::ys)) = x * y + korv (xs, ys)





fun multi (_,[]) = [] 
  | multi (x,(y::ys)) = korv (x,y) :: multi (x, ys)


fun gg ([], _) = [] 
| gg ((x::xs), (y::ys)) = multi (x, y::ys) :: gg (xs, (y::ys))





fun multiply (x,y) = gg (x, flipp y)








fun movePos (x, y) = List.drop (x, y) @ List.take (x, y) 

fun moveNeg (x, y) = List.drop (x, length x - y) @ List.take (x, length x - y)
(*
fun bajs ([],_) = [] 
| bajs ((x::xs), y) = moveNeg (x, y) :: bajs(xs, y + 1)

fun bajs' x = bajs (x, 0)
*)



fun move (f, [], _) = []
| move (f, (x::xs), y) = f (x, y) :: move(f, xs, y + 1)

fun move' (f, x) = move (f, x, 0)







val mat = [[1,2,3],[4,5,6],[7,8,9]]


val matris = [[1,2],[5,6],[1,2],[1,2]]
val matris1 = [[1,2,3,4],[7,8,8,12]]

val h = [[1, 7], [2, 8]]

val kk = ([1,2,3,4],[1,3,5,7])







fun mult' [] = 1
  | mult' (x::xs) = x * mult' xs 



fun mult [] = 0 
  | mult (x::xs) = mult' x + mult xs




fun determinant x =  mult (flipp (move'(movePos, flipp x))) - mult (move'(moveNeg, flipp x))
