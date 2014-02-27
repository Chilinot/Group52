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

abstype matrix = Matrix of fractal list list with

    fun createMatrix(m) = Matrix(m)
    
    fun emptyMatrix() = Matrix([])
    
    fun addRow(Matrix([]), _) = raise Fail "addRow recieved an empty matrix!"
      | addRow(Matrix(mat as(r::m)), l) = 
        if length l = length r then
            Matrix(l::mat)
        else
            raise Fail "Length of l is not equal to the width of the matrix!" (* This is to avoid problems with the matrix getting out of shape *)
    
    fun addColumn(Matrix(m), l) = 
        if length m = length l then
            Matrix(flipp(l::(flipp(m))))
        else
            raise Fail "Length of l is not equal to the height of the matrix!" (* Same as for addRow *)
        
    fun matrixToString(Matrix(m)) = 
        let
            fun matrixToString'([])   = ""
              | matrixToString'(e::r) = fracToString(e) ^ " " ^ matrixToString'(r)
            
            fun matrixToString''([])   = ""
              | matrixToString''(r::m) = matrixToString'(r) ^ "\n" ^ matrixToString''(m)
        in
            matrixToString''(m)
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
end





val m = createMatrix([[toFractal(1),toFractal(2),toFractal(3)],[toFractal(4),toFractal(5),toFractal(6)],[toFractal(7),toFractal(8),toFractal(9)]]);

