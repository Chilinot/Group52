
(*
additions och subtraktionsfunktioner som du kallar genom funktionen operation
*)
(* 
fun add ([], []) = []
  | add ((x::xs), (y::ys)) = x + y :: add (xs, ys)


fun sub ([], []) = []
  | sub ((x::xs), (y::ys)) = x - y :: sub (xs, ys) *)
 


fun operation (f, [], []) = [] 
  | operation (f, (x::xs), (y::ys)) = f (x, y) :: operation (f, xs, ys)
  
  
fun add (l1, l2) = operation(op +, l1, l2)

fun sub (l1, l2) = operation(op -, l1, l2)


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



fun move (f, [], _) = []
| move (f, (x::xs), y) = f (x, y) :: move(f, xs, y + 1)

fun move' (f, x) = move (f, x, 0)







val mat = [[1,2,3],[4,5,6],[7,8,9]]


val matris = [[1,2],[5,6],[1,2],[1,2]]
val matris1 = [[1,2,3,4],[7,8,8,12]]

val h = [[1, 7], [2, 8]]

val kk = ([1,2,3,4],[1,3,5,7])



fun line (x, y) = List.take (x, y - 1) @ List.drop (x, y)



fun mult' [] = 1
  | mult' (x::xs) = x * mult' xs 



fun mult [] = 0 
  | mult (x::xs) = mult' x + mult xs


fun determinant ((y::ys::yss)::(x::xs::xss)::zs) = if length ((y::ys::yss)::(x::xs::xss)::zs) = 2 then y * xs - ys * x else mult (flipp (move'(movePos, flipp ((y::ys::yss)::(x::xs::xss)::zs)))) - mult (flipp (move'(moveNeg, flipp ((y::ys::yss)::(x::xs::xss)::zs))))


(* -------------------------------------------------------------------------------------------------

cofactor & adjoint functions modu argumentet är för att hålla koll på ifall det ska vara +eller-

 *)

fun cofactor' (first::matrix, newMatrix, (x,y), (xPos,yPos), 2,modu)  = 
    if (modu mod 2) = 0 then
        (determinant(first::matrix),modu+1)
    else
        (determinant(first::matrix)*(~1),modu+1)
  | cofactor' (first::matrix, newMatrix, (x,y), (xPos,yPos), i,modu)  =
    if yPos = y then
        cofactor'(flipp(rev(newMatrix)@matrix), [], (y,x),(yPos,xPos),i+1,modu)
    else
        cofactor'(matrix, first::newMatrix,(x,y),(xPos,yPos+1),i,modu)

fun cofactor'' (matrix,list,y,0,modu) = (list,modu)
  | cofactor'' (matrix, list, y, i,modu) = 
    let
        val (element,m) = cofactor'(matrix,[],(i,y),(1,1),0,modu)
    in
        cofactor''(matrix, element::list,y,i-1,m)
    end 


fun cofactor''' (matrix, newMatrix,0,modu) = newMatrix  
  | cofactor''' (matrix as (first::rest), newMatrix,y,modu) = 
    let
        val (element,m) = cofactor''(matrix,[],y,length(first),modu)
    in
        cofactor'''(matrix, element::newMatrix,y-1,(length(matrix)-y)-1)
    end
    

fun cofactor (matrix) = cofactor'''(matrix, [], length(matrix),0)

fun adjoint (matrix) = flipp(cofactor(matrix))

val test = [[1,2,3],[0,4,5],[1,0,6]]
val test2 = [[1,4,~1,0],[2,3,5,~2],[0,3,1,6],[3,0,2,1]]

val kkk = [[5,5,5],[4,0,0],[1,0,1]]

fun determinant x =  mult (flipp (move'(movePos, flipp x))) - mult (flipp (move'(moveNeg, flipp x)))







fun invers ([], y) = [] 
  | invers (x, y) = if length x < y then 
			[]
		    else
			determinant (line ((flipp x), y)) :: invers(x, y + 1) 





fun invers' ([],_) = []
  | invers' (x, y) = if length x < y then 
			 []
		     else 
			 invers (line (x, y), 1) :: invers' (x, y + 1)


val mar = [[1,0,1],[5,4,9],[3,7,0]]

val kar = [[4,9],[7,0]]

