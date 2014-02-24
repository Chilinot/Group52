
abstype matrix = Matrix of int list list where

    fun empty() = Matrix([[]])

    fun matrixOperator(f, _, []) = []
      | matrixOperator(f, [], _) = []
      | matrixOperator(f, r1::m1, r2::m2) = 
        let
            fun matrixOperator'(f, _, []) = []
              | matrixOperator'(f, [], _) = []
              | matrixOperator'(f, x1::r1, x2::r2) = f(x1,x2) :: matrixOperator(f, r1, r2)
        in
            matrixOperator'(f, r1, r2) :: matrixOperator(f, m1, m2)
        end

    fun mAdd(m1, m2) = matrixOperator(op +, m1, m2)

    fun mSub(m1, m2) = matrixOperator(op -, m1, m2)
    
    fun mMult() = 
end





fun test 1 = 
    let
    in
    end
