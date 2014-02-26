(* 
abstype matrix = Matrix of int list list where

    fun empty() = Matrix([[]])

    fun mOp(f, _, []) = []
      | mOp(f, [], _) = []
      | mOp(f, r1::m1, r2::m2) = 
        let
            fun mOp'(f, _, []) = []
              | mOp'(f, [], _) = []
              | mOp'(f, x1::r1, x2::r2) = f(x1,x2) :: mOp(f, r1, r2)
        in
            mOp'(f, r1, r2) :: mOp(f, m1, m2)
        end

    fun mAdd(m1, m2) = mOp(op +, m1, m2)

    fun mSub(m1, m2) = mOp(op -, m1, m2)
    
    fun mMult() = 
end





fun test 1 = 
    let
    in
    end
 *)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 