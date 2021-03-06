use "fractal.sml";

(*
    delete l
    TYPE:    'a list list -> 'a list list
    PRE:     True
    POST:    l, where each element in l has had their head elements removed.
    EXAMPLE: 
        Remove the head elements in the list list [[1,2,3], [4,5,6], [7,8,9]]:
            delete([[1,2,3], [4,5,6], [7,8,9]]) = [[2,3],[5,6],[8,9]]
*)
(*  VARIANT: Length of l. *)
fun delete([])     = []
  | delete([]::xs) = [] :: delete xs
  | delete(x::xs)  = tl x :: delete xs

(*
    flipp l
    TYPE:   'a list list -> 'a list list
    PRE:    True
    POST:   Each column of elements in l has become separate rows.
    EXAMPLE:
        Flipp a 2-D list with the following structure: [[1, 2, 3],
                                                        [4, 5, 6],
                                                        [7, 8, 9]]
                                                        
            flipp([[1,2,3],[4,5,6],[7,8,9]]) = [[1,4,7],[2,5,8],[3,6,9]]
*)
(*  VARIANT: Length of l. *)
fun flipp([])    = []
  | flipp([]::x) = []
  | flipp(x)     = 
    let
        (*
            flipp' l
            TYPE:   'a list list -> 'a list
            PRE:    True
            POST:   A list, where each element are the heads of the elements in the argument l.
        *)
        (*  VARIANT: Length of l. *)
        fun flipp'([])     = []
          | flipp'([]::xs) = []
          | flipp'(x::xs)  = hd x :: flipp' xs
    in
        flipp' x :: flipp (delete x)
    end
    
(*
    createList(e, n)
    TYPE:   'a * int -> 'a list
    PRE:    True
    POST:   List containing n amount of e.
    EXAMPLE:
        Create a list of 1's with a length of 5:
            createList(1, 5) = [1,1,1,1,1]
        Create a list of "s"'s with a length of 3:
            createList("s", 3) = ["s", "s", "s"]
*)
(*  VARIANT: Size of n. *)
fun createList(_, 0) = []
  | createList(e, n) = e :: createList(e, n-1)

(*
    dropNth(l, n)
    TYPE:   'a list * int -> 'a list
    PRE:    0 < n <= length l
    POST:   List l, where the n'th element has been removed. 
            The first element in the list is regarded as having position 1.
    EXAMPLE:
        Remove the first element in the list [1,2,3,4]:
            dropNth([1,2,3,4], 1) = [2,3,4]
*)
fun dropNth (l, n) = List.take (l, n - 1) @ List.drop (l, n)

(*
    REPRESENTATION CONVENTION: 
        Represents a basic fractal-matrix.
        
        PARAMETERS:
            Matrix(m):
                m - A 2D-fractal list, where the top list contains the row lists.
                    Each element in the top list is a row in the matrix.
    
    REPRESENTATION INVARIANT: 
        None.
*)
abstype matrix = Matrix of fractal list list with

    (*
        createMatrix m
        TYPE:   fractal list list -> matrix
        PRE:    True
        POST:   A fractal-matrix based on m.
    *)
    fun createMatrix(m) = Matrix(m)
        
    (*
        matrixToString m
        TYPE:   matrix -> string
        PRE:    True
        POST:   String representing the matrix where each row is terminated with a line break.
    *)
    (*  VARIANT: Height of matrix m. *)
    fun matrixToString(Matrix([]))   = ""
      | matrixToString(Matrix(r::m)) = 
        let
            (*
                matrixToString' r
                TYPE:   fractal list -> string
                PRE:    True
                POST:   String representing each fractal in the list r.
            *)
            (*  VARIANT: Length of r. *)
            fun matrixToString'([])   = ""
              | matrixToString'(e::r) = fracToString(e) ^ "\t" ^ matrixToString'(r)
        in
            matrixToString'(r) ^ "\n" ^ matrixToString(Matrix(m))
        end
        
    (*
        mOp(f, m1, m2)
        TYPE:   (fractal * fractal -> fractal) * matrix * matrix -> matrix
        PRE:    True
        POST:   Matrix, where each element with the same positions in m1 and m2 has had the function f applied to them.
    *)
    fun mOp(f, m1, m2) = 
        let
            (*
                mOp'(f, l1, l2)
                TYPE:   ('a * 'b -> 'c) * 'a list * 'b list -> 'c list
                PRE:    True
                POST:   List of results from the function f being applied to for each element in the lists l1 and l2.
            *)
            (*  VARIANT: Length of l1 and l2. *)
            fun mOp'(_, _, []) = []
              | mOp'(_, [], _) = []
              | mOp'(f, e1::r1, e2::r2) = f(e1, e2) :: mOp'(f, r1, r2)
              
            (*
                mOp''(f, m1, m2)
                TYPE:   ('a * 'b -> 'c) * matrix * matrix -> 'c list list
                PRE:    True
                POST:   List of results from the mOp' function, where the arguments passed are the function f, and each row in the matrixes m1 and m2.
            *)
            (*  VARIANT: Height of m1 and m2. *)
            fun mOp''(_, _, Matrix([])) = []
              | mOp''(_, Matrix([]), _) = []
              | mOp''(f, Matrix(r1::m1), Matrix(r2::m2)) = mOp'(f, r1, r2) :: mOp''(f, Matrix(m1), Matrix(m2))
        in
            Matrix(mOp''(f, m1, m2))
        end
        
    (*
        mAdd(m1, m2)
        TYPE:   matrix * matrix -> matrix
        PRE:    True
        POST:   The result of the two matrixes m1 and m2 added to eachother.
    *)
    fun mAdd(m1, m2) = mOp(fracAdd, m1, m2)
    
    (*
        mSub(m1, m2)
        TYPE:   matrix * matrix -> matrix
        PRE:    True
        POST:   The result of the two matrixes m1 and m2 subtracted from eachother.
    *)
    fun mSub(m1, m2) = mOp(fracSub, m1, m2)
    
    (*
        mMult(m1, m2)
        TYPE:   matrix * matrix -> matrix
        PRE:    True
        POST:   The result of the two matrixes m1 and m2 multiplied with eachother.
    *)
    fun mMult(Matrix(m1), Matrix(m2)) =
        let
            (*
                multi'(l1, l2)
                TYPE:   fractal list * fractal list -> fractal
                PRE:    True
                POST:   The sum of each value in the two lists l1 and l2 being multiplied together and added to the rest of the products.
            *)
            (*  VARIANT: Length of l1 and l2. *)
            fun multi'([], _) = toFractal(0)
              | multi'(_, []) = toFractal(0)
              | multi'((x::xs), (y::ys)) = fracAdd(fracMult(x, y), multi'(xs, ys))

            (*
                multi(l1, l2)
                TYPE:   fractal list * fractal list list -> fractal list
                PRE:    True
                POST:   Returns list of elements from l1 multiplied with l2
            *)
            (*  VARIANT: Length of l2 *)
            fun multi([], _)      = []
              | multi(_, [])      = [] 
              | multi(x, (y::ys)) = multi' (x,y) :: multi (x, ys)

            (*
                multiply' (l1, l2)
                TYPE:   fractal list list * fractal list list -> fractal list list
                PRE:    True
                POST:   l1 and the transpose of l2 multiplied with each other in the same way as matrix multiplication
            *)
            (*  VARIANT: length of m *)
            fun multiply'(_, []) = []
              | multiply'([], _) = [] 
              | multiply'((x::xs), (y::ys)) = multi (x, y::ys) :: multiply' (xs, (y::ys))

            (*
                multiply(l1, l2)
                TYPE:   fractal list list * fractal list list -> fractal list list
                PRE:    True
                POST:   l1 and l2 multiplied with each other in the same way as matrix multiplication.
            *)
            fun multiply(x, []) = x
              | multiply(x, y) = multiply' (x, flipp y)
        in
            Matrix(multiply (m1, m2))
        end 
        
    (*
        mFractMult(f, m)
        TYPE:   fractal * matrix -> matrix
        PRE:    Matrix m is a square matrix.
        POST:   Matrix corresponding to the matrix m being multiplied with the fractal f.
    *)
    fun mFractMult(f, matrix as (Matrix(m))) = 
        let
            val height = length m
            val width  = if height = 0 then 0 else length(hd m)
        in
            mOp(fracMult, Matrix(createList(createList(f, width), height)), matrix)
        end
        
    (*
        mDet m
        TYPE:   matrix -> fractal
        PRE:    Matrix m is a non-empty square matrix. 
        POST:   Fractal corresponding to the determinant of the matrix m.
    *)
    fun mDet(Matrix([])) = raise Fail "mDet can not determine the determinant of an empty matrix!"
      | mDet(Matrix(m))  = 
        let
            (*
                mDet'(m, k)
                TYPE:   fractal list list * int -> fractal
                PRE:    m is non-empty and k is equal to 1.
                POST:   The determinant of the 2D-fractal list m.
            *)
            (*  VARIANT: k *)
            fun mDet'([[x]],   k) = x
              | mDet'((x::xs), k) = 
                if k > length x then 
                    toFractal(0)
                else
                    let
                        val s  = if k mod 2 = 0 then toFractal(~1) else toFractal(1) (* Determine sign for the cofactor. *)
                        val c  = fracMult(s, List.nth(x, k - 1))                     (* Retrieve the cofactor. *)
                        val d1 = mDet'(flipp (dropNth ((flipp xs), k)), 1)           (* Retrieve the determinant of the minor. *)
                        val d2 = mDet'((x::xs), k + 1)                               (* Retrieve the determinant for the rest of the matrix. *)
                    in
                        fracAdd(fracMult(c, d1), d2)                                 (* Multiply the cofactor with the determinant of the minor and add the determinant of the rest of the matrix. *)
                    end
        in
            mDet'(m, 1)
        end
    
    (*
        mCofactor (m)
        TYPE:   Matrix -> Matrix
        PRE:    Square matrix(nXn)
        POST:   Returns the cofactor matrix of m
    *)
    fun mCofactor (Matrix(m)) = 
        let
            (*
                cofactor' (matrix1, matrix2,(searchPosX,searchPosY), (currentX,currentY),i,modu)
                TYPE:   int list list * int list list * (int * int) * (int * int) * int * int -> (int * int)
                PRE:    x<length matrix1
                POST:   Returns the cofactor value of the element in matrix at (searchPosX,searchPosY), and the modulo current value.
            *)
            (*  VARIANT: searchPosX+searchPosY *)
            fun cofactor' (first::matrix, newMatrix, (x,y), (xPos,yPos), 2,modu)  = 
                if (modu mod 2) = 0 then
                    (mDet(Matrix(first::matrix)), modu+1)
                else
                    (fracMult(mDet(Matrix(first::matrix)), toFractal(~1)),modu+1)
              | cofactor' (first::matrix, newMatrix, (x,y), (xPos,yPos), i,modu)  =
                if yPos = y then
                    cofactor'(flipp(rev(newMatrix)@matrix), [], (y,x),(yPos,xPos),i+1,modu)
                else
                    cofactor'(matrix, first::newMatrix,(x,y),(xPos,yPos+1),i,modu)

            (*
                cofactor''(matrix,list,y,i,modu)
                TYPE:   int list list * int list * int * int * int -> (int list * int)
                PRE:    true
                POST:   Returns the cofactor row y of matrix, and current modulus value
            *)
            (*  VARIANT: i *)
            fun cofactor'' (matrix,list,y,0,modu) = (list,modu)
              | cofactor'' (matrix, list, y, i,modu) = 
                let
                    val (element,m) = cofactor'(matrix,[],(i,y),(1,1),0,modu)
                in
                    cofactor''(matrix, element::list,y,i-1,m)
                end 

            (*
                cofactor'''(matrix,newMatrix, y, modu)
                TYPE:   int list list * int list list * int * int -> int list list
                PRE:    true
                POST:   Returns the cofactor matrix of matrix
            *)
            (*  VARIANT: length of matrix *)
            fun cofactor''' (matrix, newMatrix,0,modu) = newMatrix  
              | cofactor''' (matrix as (first::rest), newMatrix,y,modu) = 
                let
                    val (element,m) = cofactor''(matrix,[],y,length(first),modu)
                in
                    cofactor'''(matrix, element::newMatrix,y-1,(length(matrix)-y)-1)
                end
        in
            Matrix(cofactor'''(m, [], length(m),0))
        end
        
    (*
        mAdjoint m
        TYPE:   matrix -> matrix
        PRE:    True
        POST:   The adjugate/adjoint of the matrix m.
    *)
    fun mAdjoint (m) = 
        let
            val Matrix(c) = mCofactor(m)
        in
            Matrix(flipp(c))
        end
        
    (*
        mInv m
        TYPE:   matrix -> matrix
        PRE:    The determinant of the matrix m is not equal to zero.
        POST:   Matrix corresponding to the inverse of the matrix m.
    *)
    fun mInv(m) = 
        let
            val det = mDet(m)
        in
            if fracEqualsZero(det) then
                raise Fail "The matrix is not invertible!"
            else
                mFractMult(fracDivide(toFractal(1), det), mAdjoint(m))
        end
    
    (*
        parseMatrix s
        TYPE:   string -> matrix
        PRE:    - Each element in the string s that is supposed to become an element in the matrix has to be represented as a fractal. I.e "1/4".
                - The string can not represent an empty matrix. I.e: "{}".
        POST:   Matrix equal to which was represented in the string s.
        EXAMPLE:
            Create a matrix with the following structure: 1 2
                                                          3 4
                parseMatrix("{{1/1, 2/1}, {3/1, 4/1}}")
    *)
    fun parseMatrix(s) =
        let
            (*
                retrieveFractal l
                TYPE:   char list -> char list
                PRE:    True
                POST:   All characters from left to right that is before any of the "}" and "," characters.
            *)
            (*  VARIANT: Length of l. *)
            fun retrieveFractal([])      = []
              | retrieveFractal(#"}"::l) = []
              | retrieveFractal(#","::l) = []
              | retrieveFractal(h::l)    = h :: retrieveFractal(l)
            
            (*
                parseRow l
                TYPE:   char list -> fractal list * char list
                PRE:    The list of chars has to be correctly formatted according to the pre in parseMatrix.
                POST:   Finished fractal list after the char list l has been parsed by the aux-functions.
                SIDE-EFFECTS: Raises Fail if the chars in l is in an illegal order.
            *)
            (*  VARIANT: Length of l. *)
            fun parseRow([]) = raise Fail "parseRow ran out of elements before it struck an end character!"
              | parseRow(h::l) = 
                if h = #"{" orelse h = #"," then  (* Skip these characters *)
                    parseRow(l)
                else if h = #"}" then             (* End character, marks the end of the recursion *)
                    ([], l)
                else
                    let
                        val f = h :: retrieveFractal(l)
                        val (l2, r) = parseRow(List.drop(l, length(f) - 1))
                    in
                        (fractalFromString(implode(f)) :: l2, r)
                    end
        
            (*
                parseMatrix' l
                TYPE:   char list -> fractal list list
                PRE:    l needs to be an exploded version of a correctly structured string according to the PRE of parseMatrix.
                POST:   Finished 2D-fractal list after the char list l has been parsed by the aux-functions.
                SIDE-EFFECTS: Raises Fail if the chars in l is in an illegal order.
            *)
            (*  VARIANT: Lenght of l. *)
            fun parseMatrix'([]) = raise Fail "parseMatrix' ran out of characters before the end character!"
              | parseMatrix'(h::s) = 
                if h = #"," then
                    parseMatrix'(s)
                else if h = #"{" then
                    let
                        val (l, r) = parseRow(s)
                    in
                        l :: parseMatrix'(r)
                    end
                else if h = #"}" then
                    []
                else
                    raise Fail "parseMatrix' seemed to have recieved an argument of illegal form!"
        in
            Matrix(parseMatrix'(List.filter (fn x => x <> #" ") (explode(s))))
        end
end

(*
    matrixTest()
    TYPE:   unit -> unit
    PRE:    None
    POST:   None
    SIDE-EFFECTS: Performs unit-tests on all functions in the abstype matrix and prints the results to the console.
*)
fun matrixTest() = 
    let
        fun test 1 = 
            let
                val m = createMatrix([[toFractal(1),toFractal(2),toFractal(3)],[toFractal(4),toFractal(5),toFractal(6)],[toFractal(7),toFractal(8),toFractal(9)]])
                val a = createMatrix([[toFractal(2),toFractal(4),toFractal(6)],[toFractal(8),toFractal(10),toFractal(12)],[toFractal(14),toFractal(16),toFractal(18)]])
                val r = mAdd(m, m)
            in
                matrixToString(r) = matrixToString(a)
            end
          | test 2 =
            let
                val m = createMatrix([[toFractal(1),toFractal(2),toFractal(3)],[toFractal(4),toFractal(5),toFractal(6)],[toFractal(7),toFractal(8),toFractal(9)]])
                val a = createMatrix([[toFractal(0),toFractal(0),toFractal(0)],[toFractal(0),toFractal(0),toFractal(0)],[toFractal(0),toFractal(0),toFractal(0)]])
                val r = mSub(m, m)
            in
                matrixToString(r) = matrixToString(a)
            end
          | test 3 =
            let
                val m = createMatrix([[toFractal(1),toFractal(2),toFractal(3)],[toFractal(4),toFractal(5),toFractal(6)],[toFractal(7),toFractal(8),toFractal(9)]])
                val a = createMatrix([[toFractal(30),toFractal(36),toFractal(42)],[toFractal(66),toFractal(81),toFractal(96)],[toFractal(102),toFractal(126),toFractal(150)]])
                val r = mMult(m, m)
            in
                matrixToString(r) = matrixToString(a)
            end
          | test 4 =
            let
                val m = createMatrix([[toFractal(1),toFractal(2),toFractal(3)],[toFractal(4),toFractal(5),toFractal(6)],[toFractal(7),toFractal(8),toFractal(9)]])
                val a = createMatrix([[toFractal(2),toFractal(4),toFractal(6)],[toFractal(8),toFractal(10),toFractal(12)],[toFractal(14),toFractal(16),toFractal(18)]])
                val r = mFractMult(toFractal(2), m)
                
                val a2 = createMatrix([[createFractal(1,2),createFractal(2,2),createFractal(3,2)],[createFractal(4,2),createFractal(5,2),createFractal(6,2)],[createFractal(7,2),createFractal(8,2),createFractal(9,2)]])
                val r2 = mFractMult(createFractal(1,2), m)
            in
                matrixToString(r) = matrixToString(a) andalso
                matrixToString(r2) = matrixToString(a2)
            end
          | test 5 =
            let
                val m = createMatrix([[toFractal(1),toFractal(2),toFractal(3)],[toFractal(4),toFractal(5),toFractal(6)],[toFractal(7),toFractal(8),toFractal(9)]])
                val a = 0.0
                val r = mDet(m)
            in
                Real.==(fracToReal(r), a)
            end
          | test 6 =
            let
                val m = createMatrix([[toFractal(1),toFractal(2),toFractal(3)],[toFractal(4),toFractal(5),toFractal(6)],[toFractal(7),toFractal(8),toFractal(9)]])
                val a = createMatrix([[toFractal(~3),toFractal(6),toFractal(~3)],[toFractal(6),toFractal(~12),toFractal(6)],[toFractal(~3),toFractal(6),toFractal(~3)]])
                val r = mCofactor(m)
            in
                matrixToString(r) = matrixToString(a)
            end
          | test 7 =
            let
                val m = createMatrix([[toFractal(1),toFractal(2),toFractal(3)],[toFractal(4),toFractal(5),toFractal(6)],[toFractal(7),toFractal(8),toFractal(9)]])
                val a = createMatrix([[toFractal(~3),toFractal(6),toFractal(~3)],[toFractal(6),toFractal(~12),toFractal(6)],[toFractal(~3),toFractal(6),toFractal(~3)]])
                val r = mAdjoint(m)
            in
                matrixToString(r) = matrixToString(a)
            end
          | test 8 = 
            let
                val m  = createMatrix([[toFractal(1),toFractal(0),toFractal(3)],[toFractal(4),toFractal(5),toFractal(6)],[toFractal(7),toFractal(8),toFractal(9)]])
                val ma = createMatrix([[toFractal(3),toFractal(~24),toFractal(15)],[toFractal(~6),toFractal(12),toFractal(~6)],[toFractal(3),toFractal(8),toFractal(~5)]])
                val a  = mFractMult(createFractal(1, 12), ma)
                val r  = mInv(m)
            in
                matrixToString(r) = matrixToString(a)
            end
          | test 9 =
            let
                val r = parseMatrix("{{1/1,2/1,3/1},{4/1,5/1,6/1}, {7/1 ,8/1, 9/1}}")
                val a = createMatrix([[toFractal(1),toFractal(2),toFractal(3)],[toFractal(4),toFractal(5),toFractal(6)],[toFractal(7),toFractal(8),toFractal(9)]])
                
                val r2 = parseMatrix("{{1/1,2/1,3/1},{4/1,5/1,6/1},{7/3,80/20,9/2}}")
                val a2 = createMatrix([[toFractal(1),toFractal(2),toFractal(3)],[toFractal(4),toFractal(5),toFractal(6)],[createFractal(7,3),toFractal(4),createFractal(9,2)]])
            in
                matrixToString(r)  = matrixToString(a) andalso
                matrixToString(r2) = matrixToString(a2)
            end
            
        fun getString(true)  = "SUCCESS"
          | getString(false) = "FAILED"
    in
        print("Test mAdd: \t\t"        ^ getString(test(1)) ^ "\n" ^
              "Test mSub: \t\t"        ^ getString(test(2)) ^ "\n" ^
              "Test mMult: \t\t"       ^ getString(test(3)) ^ "\n" ^
              "Test mFractMult: \t\t"  ^ getString(test(4)) ^ "\n" ^
              "Test mDet: \t\t"        ^ getString(test(5)) ^ "\n" ^
              "Test mCofactor: \t\t"   ^ getString(test(6)) ^ "\n" ^
              "Test mAdjoint: \t\t"    ^ getString(test(7)) ^ "\n" ^
              "Test mInv: \t\t"        ^ getString(test(8)) ^ "\n" ^
              "Test parseMatrix: \t\t" ^ getString(test(9)) ^ "\n" )
    end 