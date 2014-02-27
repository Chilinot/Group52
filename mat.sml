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
    
    fun emptyMatrix() = Matrix([])
    
    fun addRow(Matrix(m), l) = Matrix(l::m)
    
    fun addColumn(Matrix(m), l) = Matrix(flipp(l::(flipp(m))))
        
    fun matrixToString(Matrix(m)) = 
        let
            fun matrixToString'([])   = ""
              | matrixToString'(e::r) = fracToString(e) ^ " " ^ matrixToString'(r)
            
            fun matrixToString''([])   = ""
              | matrixToString''(r::m) = matrixToString'(r) ^ "\n" ^ matrixToString''(m)
        in
            matrixToString''(m)
        end
end
