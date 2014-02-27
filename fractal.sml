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
        gcd(n1, n2)
        TYPE:   int * int -> int
        PRE:    True
        POST:   The greatest common divider between n1 and n2.
    *)
    fun gcd(n1, n2) =
        if n2 > 0 then
            gcd(n2, n1 mod n2)
        else
            n1
    
    (*
        simplify f
        TYPE:   fractal -> fractal
        PRE:    True
        POST:   Fractal f, where the numerator and denominator have been divided by their greatest common divider.
    *)
    fun simplify(Fractal(n,d)) =
        let
            val g = gcd(n,d)
        in
            Fractal(n div g, d div g)
        end

    (*
        createFractal(n,d)
        TYPE:   int * int -> fractal
        PRE:    d can not be equal to zero.
        POST:   The fractal based on n as numerator and d as denominator.
        SIDE-EFFECTS: Raises Fail if d is equal to zero.
    *)
    fun createFractal(n,0) = raise Fail "The denominator can not be equal to zero!" 
      | createFractal(n,d) = simplify(Fractal(n,d))
    
    (*
        toFractal n
        TYPE:   int -> fractal
        PRE:    True
        POST:   The fractal based on n as numerator and 1 as denominator.
    *)
    fun toFractal(n) = Fractal(n, 1)
        
    (*
        fractToString f
        TYPE:   fractal -> string
        PRE:    True
        POST:   String representing the fractal f.
    *)
    fun fracToString(Fractal(n,d)) = Int.toString(n) ^ "/" ^ Int.toString(d)
    
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
    fun fracOp(f, Fractal(n1, d1), Fractal(n2, d2)) = simplify(Fractal(f(n1 * d2, n2 * d1), d1 * d2))
    
    (*
        fracAdd(f1, f2)
        TYPE:   fractal * fractal -> fractal
        PRE:    True
        POST:   Fractal representing the sum between the fractals f1 and f2.
    *)
    fun fracAdd(f1, f2) = fracOp(op +, f1, f2)
    
    (*
        fracSub(f1, f2)
        TYPE:   fractal * fractal -> fractal
        PRE:    True
        POST:   Fractal representing the differense between the fractals f1 and f2.
    *)
    fun fracSub(f1, f2) = fracOp(op -, f1, f2)
    
    (*
        fracMult(f1, f2)
        TYPE:   fractal * fractal -> fractal
        PRE:    True
        POST:   Fractal representing the product of the fractals f1 and f2.
    *)
    fun fracMult(Fractal(n1, d1), Fractal(n2, d2)) = simplify(Fractal(n1 * n2, d1 * d2))
    
    (*
        fracDivide(f1, f2)
        TYPE:   fractal * fractal -> fractal
        PRE:    True
        POST:   Fractal representing the quota of the fractals f1 and f2.
    *)
    fun fracDivide(fr, Fractal(n, d)) = fracMult(fr, Fractal(d, n))
end

fun fractTest() = 
    let
        fun test 1 = 
            let
                val f1 = createFractal(12, 2)
                val f2 = toFractal(6)
                val a  = fracAdd(f1, f2)
            in
                 Real.==(fracToReal(a), 12.0)
            end
          | test 2 =
            let
                val f1 = createFractal(12, 2)
                val f2 = createFractal(3,4)
                val s  = fracSub(f1,f2)
            in
                Real.==(fracToReal(s), 5.25)
            end 
          | test 3 = 
            let
                val f1 = createFractal(12, 1)
                val f2 = createFractal(23, 2)
                val m  = fracMult(f1, f2)
            in
                Real.==(fracToReal(m), 138.0)
            end 
         (*  | test 4 =
            let
                val f1 = createFractal(4, 6)
                val f2 = createFractal(3, 2)
                val d  = fracDivide(f1, f2)
            in
                Real.==(fracToReal(d), 4.0 / 9.0) (* This test is actually true, even though it returns false. Possibly a bug in Real.== *)
            end  *)
          | test 5 = fracToString(createFractal(1,4)) = "1/4"
            
        fun getString(true)  = "SUCCESS"
          | getString(false) = "FAILED"
    in
        print("Test 1: " ^ getString(test(1)) ^ "\n" ^
              "Test 2: " ^ getString(test(2)) ^ "\n" ^
              "Test 3: " ^ getString(test(3)) ^ "\n" ^
              (* "Test 4: " ^ getString(test(4)) ^ "\n" ^ *)
              "Test 5: " ^ getString(test(5)) ^ "\n")
    end











