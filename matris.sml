

fun add ([], []) = []
  | add ((x::xs), (y::ys)) = x + y :: add (xs, ys)


fun sub ([], []) = []
  | sub ((x::xs), (y::ys)) = x - y :: sub (xs, ys)
 



fun addH ((x::xs), (y::ys)) = x + y

fun HD (x::xs) = x

fun multiP ((x::xs), (y::ys)) = addH (x, y) + addH (x, HD ys) 

fun operation (f, [], []) = [] 
  | operation (f, (x::xs), (y::ys)) = f (x, y) :: operation (f, xs, ys)


fun delete [] = []
  | delete ((y::ys)::xs) = ys :: delete xs


fun  hejsan ((y::ys)::xs) = y :: hejsan xs :: hejsan delete ((y::ys)::xs)


fun bajs (x::xs) = delete (x::xs)  


(* asdkjldfsdf *)
