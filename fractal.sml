(*
    REPRESENTATION CONVENTION: 
        Represents a fractal.
        
        PARAMETERS:
            Fractal(numerator, denominator):
                numerator   - The numerator of the fractal.
                denominator - The denominator of the fractal.
    
    REPRESENTATION INVARIANT: 
        The denominator can not be equal to zero.
*)
abstype fractal = Fractal of int * int with
    
    (*
        createFractal(n,d)
        TYPE:   int * int -> fractal
        PRE:    d can not be equal to zero.
        POST:   The fractal based on n as numerator and d as denominator.
        SIDE-EFFECTS: Raises Fail if d is equal to zero.
    *)
    fun createFractal(n,0) = raise Fail "The denominator can not be equal to zero!" 
      | createFractal(n,d) = Fractal(n,d)
    
    (*
        toFractal n
        TYPE:   int -> fractal
        PRE:    True
        POST:   The fractal based on n as numerator and 1 as denominator.
    *)
    fun toFractal(n) = Fractal(n, 1)
    
    (*
        fracToReal f
        TYPE:   fractal -> real
        PRE:    True
        POST:   The real value from the division of the numerator and denominator.
        SIDE-EFFECTS: Raises Fail if the fractal has a zero denominator.
    *)
    fun fracToReal(Fractal(n,0)) = raise Fail "fracToReal recieved a fractal with a denominator equal to zero!"
      | fracToReal(Fractal(n,d)) = Real.fromInt(n) / Real.fromInt(d)
    
    (*
        fracOp(f, f1, f2)
        TYPE:   (int * int -> int) * fractal * fractal -> fractal
        PRE:    True
        POST:   
    *)
    fun fracOp(f, Fractal(n1, d1), Fractal(n2, d2)) = Fractal(f(n1 * d2, n2 * d1), d1 * d2)
    
    (*
        add(f1, f2)
        TYPE:   fractal * fractal -> fractal
        PRE:    True
        POST:   Fractal representing the sum between the fractals f1 and f2.
    *)
    fun add(f1, f2) = fracOp(op +, f1, f2)
    
    (*
        sub(f1, f2)
        TYPE:   fractal * fractal -> fractal
        PRE:    True
        POST:   Fractal representing the differense between the fractals f1 and f2.
    *)
    fun sub(f1, f2) = fracOp(op -, f1, f2)
    
    (*
        mult(f1, f2)
        TYPE:   fractal * fractal -> fractal
        PRE:    True
        POST:   Fractal representing the product of the fractals f1 and f2.
    *)
    fun mult(Fractal(n1, d1), Fractal(n2, d2)) = Fractal(n1 * n2, d1 * d2)
    
    (*
        divide(f1, f2)
        TYPE:   fractal * fractal -> fractal
        PRE:    True
        POST:   Fractal representing the quota of the fractals f1 and f2.
    *)
    fun divide(fr, Fractal(n, d)) = mult(fr, Fractal(d, n))
end

fun fractTest() = 
    let
        fun test 1 = 
            let
                val f1 = createFractal(12, 2)
                val f2 = toFractal(6)
                val a  = add(f1, f2)
            in
                 Real.==(fracToReal(a), 12.0)
            end
          | test 2 =
            let
                val f1 = createFractal(12, 2)
                val f2 = createFractal(3,4)
                val s  = sub(f1,f2)
            in
                Real.==(fracToReal(s), 5.25)
            end 
          | test 3 = 
            let
                val f1 = createFractal(12, 1)
                val f2 = createFractal(23, 2)
                val m  = mult(f1, f2)
            in
                Real.==(fracToReal(m), 138.0)
            end 
          | test 4 =
            let
                val f1 = createFractal(4, 6)
                val f2 = createFractal(3, 2)
                val d  = divide(f1, f2)
            in
                Real.==(fracToReal(d), 8.0 / 18.0)
            end
            
        fun getString(true)  = "SUCCESS"
          | getString(false) = "FAILED"
    in
        print("Test 1: " ^ getString(test(1)) ^ "\n" ^
              "Test 2: " ^ getString(test(2)) ^ "\n" ^
              "Test 3: " ^ getString(test(3)) ^ "\n" ^
              "Test 4: " ^ getString(test(4)) ^ "\n")
    end











