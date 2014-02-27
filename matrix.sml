use "mat.sml";


(*
additions och subtraktionsfunktioner som du kallar genom funktionen operation
*)

(* fun operation (f, [], []) = [] 
  | operation (f, (x::xs), (y::ys)) = f (x, y) :: operation (f, xs, ys)
  
fun add (l1, l2) = operation(op +, l1, l2)

fun sub (l1, l2) = operation(op -, l1, l2) *)

(*
--------------------------------------------------------

funktionen flipp som flippar runt listan innan multiplikationen
*)


(* fun delete [] = []
  | delete (x::xs) = tl x :: delete xs

fun flipp [] = [] 
  | flipp x = 
    let
        fun flipp' [] = []
          | flipp' (x::xs) = hd x :: flipp' xs
    in
        if hd x = [] then 
		    [] 
        else 
            flipp' x :: flipp (delete x)
    end *)

(* -------------------------------------------------------------------------------------------------

cofactor & adjoint functions modu argumentet är för att hålla koll på ifall det ska vara +eller-

 *)

(* fun cofactor (matrix) = 
    let
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
    in
        cofactor'''(matrix, [], length(matrix),0)
    end

fun adjoint (matrix) = flipp(cofactor(matrix))

val test = [[1,2,3],[0,4,5],[1,0,6]]
val test2 = [[1,4,~1,0],[2,3,5,~2],[0,3,1,6],[3,0,2,1]] *)