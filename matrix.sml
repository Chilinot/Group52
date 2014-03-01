use "fractal.sml";

fun delete([])    = []
  | delete(x::xs) = tl x :: delete xs

fun flipp([])    = []
  | flipp([]::x) = []
  | flipp(x)     = 
    let
        fun flipp'([])     = []
          | flipp'([]::xs) = []
          | flipp'(x::xs)  = hd x :: flipp' xs
    in
        flipp' x :: flipp (delete x)
    end
    
fun line (x, y) = List.take (x, y - 1) @ List.drop (x, y)

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
        addRow(m, l)
        TYPE:   matrix * fractal list -> matrix
        PRE:    Length of l has to be the same as the width of the matrix m. Matrix m can not be empty.
        POST:   Matrix m with the fractal list l added as the top row.
        SIDE-EFFECTS: Raises Fail if the matrix m is empty or the list l does not have a length equal to the width of the matrix.
    *)
    fun addRow(Matrix([]), _) = raise Fail "addRow recieved an empty matrix!"
      | addRow(Matrix(mat as(r::m)), l) = 
        if length l = length r then
            Matrix(l::mat)
        else
            raise Fail "Length of l is not equal to the width of the matrix!" (* This is to avoid problems with the matrix getting out of shape *)
    
    (* 
        addColumn(m, l)
        TYPE:   matrix * fractal list -> matrix
        PRE:    Length of l has to be the same as the height of the matrix m.
        POST:   Matrix m with the added fractal list l as the leftmost column.
        SIDE-EFFECTS: Raises Fail if the length of list l is not equal to the heigth of the matrix.
    *)
    fun addColumn(Matrix(m), l) = 
        if length m = length l then
            Matrix(flipp(l::(flipp(m))))
        else
            raise Fail "Length of l is not equal to the height of the matrix!" (* Same as for addRow *)
        
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
              | matrixToString'(e::r) = fracToString(e) ^ " " ^ matrixToString'(r)
        in
            matrixToString'(r) ^ "\n" ^ matrixToString(Matrix(m))
        end
        
    fun mOp(f, m1, m2) = 
        let
            fun mOp'(_, _, []) = []
              | mOp'(_, [], _) = []
              | mOp'(f, e1::r1, e2::r2) = f(e1, e2) :: mOp'(f, r1, r2)
              
            fun mOp''(_, _, Matrix([])) = []
              | mOp''(_, Matrix([]), _) = []
              | mOp''(f, Matrix(r1::m1), Matrix(r2::m2)) = mOp'(f, r1, r2) :: mOp''(f, Matrix(m1), Matrix(m2))
        in
            Matrix(mOp''(f, m1, m2))
        end
        
    fun mAdd(m1, m2) = mOp(fracAdd, m1, m2)
    
    fun mSub(m1, m2) = mOp(fracSub, m1, m2)
    
    fun mMult(Matrix(m1), Matrix(m2)) =
        let
            fun multi'([], _) = toFractal(0)
              | multi'(_, []) = toFractal(0)
              | multi'((x::xs), (y::ys)) = fracAdd(fracMult(x, y), multi'(xs, ys))

            fun multi(_, [])      = [] 
              | multi(x, (y::ys)) = multi' (x,y) :: multi (x, ys)

            fun multiply'([], _) = [] 
              | multiply'((x::xs), (y::ys)) = multi (x, y::ys) :: multiply' (xs, (y::ys))

            fun multiply(x, []) = x
              | multiply(x, y) = multiply' (x, flipp y)
        in
            Matrix(multiply (m1, m2))
        end 
        
    fun mFractMult(f, Matrix(m)) = 
        let
            fun mFractMult'(_,  [])  = []
              | mFractMult'(f, e::r) = fracMult(f, e) :: mFractMult'(f, r)
              
            fun mFractMult''(_, [])   = []
              | mFractMult''(f, r::m) = mFractMult'(f, r) :: mFractMult''(f, m)
        in
            Matrix(mFractMult''(f, m))
        end
        
    fun mDet(Matrix([])) = raise Fail "mDet can not determine the determinant of an empty matrix!"
      | mDet(Matrix(m))  = 
        let
            fun mDet'([[x]],   k) = x
              | mDet'((x::xs), k) = 
                if k > length x then 
                    toFractal(0)
                else
                    let
                        val s  = if k mod 2 = 0 then toFractal(~1) else toFractal(1) (* Determine sign for the cofactor. *)
                        val c  = fracMult(s, List.nth(x, k - 1))                     (* Retrieve the cofactor. *)
                        val d1 = mDet'(flipp (line ((flipp xs), k)), 1)              (* Retrieve the determinant of the minor. *)
                        val d2 = mDet'((x::xs), k + 1)                               (* Retrieve the determinant for the rest of the matrix. *)
                    in
                        fracAdd(fracMult(c, d1), d2)                                 (* Multiply the cofactor with the determinant of the minor and add the determinant of the rest of the matrix. *)
                    end
        in
            mDet'(m, 1)
        end
        
    fun mCofactor (Matrix(m)) = 
        let
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
            Matrix(cofactor'''(m, [], length(m),0))
        end
        
    fun mAdjoint (m) = 
        let
            val Matrix(c) = mCofactor(m)
        in
            Matrix(flipp(c))
        end
        
    fun mInv(m) = 
        let
            val det = mDet(m)
        in
            if fracEqualsZero(det) then
                raise Fail "The matrix is not invertible!"
            else
                mFractMult(fracDivide(toFractal(1), det), mAdjoint(m))
        end
end



val m = createMatrix([[toFractal(1),toFractal(2),toFractal(3)],[toFractal(4),toFractal(5),toFractal(6)],[toFractal(7),toFractal(8),toFractal(9)]]);

val m2 = createMatrix([[toFractal(1),toFractal(2),toFractal(3)],[toFractal(0),toFractal(4),toFractal(5)],[toFractal(1),toFractal(0),toFractal(6)]])