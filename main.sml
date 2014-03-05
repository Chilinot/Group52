use "matrix.sml";

(*
    readSTD
    TYPE:   unit -> string
    PRE:    None
    POST:   Input read from stdIn
    SIDE-EFFECTS: Reads from stdIn
*)
fun readSTD () = valOf(TextIO.inputLine TextIO.stdIn)

(*
    twoMatrix(m1, m2)
    TYPE:   matrix * matrix -> string
    PRE:    True
    POST:   String of the result matrix from m1 and m2, determined by the users choice.
    SIDE-EFFECTS: Reads from stdIn and prints to the console.
*)
fun twoMatrix(m1, m2) = (
        print(
            "\nWhat would you like to calculate?\n" ^
            "1. Addition \n" ^
            "2. Subtraction \n" ^
            "3. Multiplication \n" ^
            "CHOICE: "
        );
        case readSTD() of
            "1\n" => matrixToString(mAdd(m1, m2))
          | "2\n" => matrixToString(mSub(m1, m2))
          | "3\n" => matrixToString(mMult(m1, m2))
          |   _   => (print("Incorrect choice! Please try again.\n"); twoMatrix(m1, m2))
    )

(*
    oneMatrix m
    TYPE:   matrix -> string
    PRE:    True
    POST:   String of the result matrix from m, determined by the users choice.
    SIDE-EFFECTS: Reads from stdIn and prints to the console.
*)
fun oneMatrix(m) = (
        print(
            "\nYou have only entered one matrix. \nWhat would you like to calculate?\n" ^
            "1. Inverse \n" ^
            "2. Determinant \n" ^
            "3. Adjoint \n" ^
            "4. Cofactor matrix \n" ^
            "CHOICE: "
        );
        case readSTD() of
            "1\n" => matrixToString(mInv(m))
          | "2\n" => fracToString(mDet(m))
          | "3\n" => matrixToString(mAdjoint(m))
          | "4\n" => matrixToString(mCofactor(m))
          |   _   => (print("Incorrect choice! Please try again.\n"); oneMatrix(m))
    )

(*
    secondMatrix m
    TYPE:   matrix -> string
    PRE:    True
    POST:   Result from the call to twoMatrix, where m is the first argument.
    SIDE-EFFECTS: Reads from stdIn and prints to the console.
*)
fun secondMatrix(m) = 
    let
        val c = (
            print "Please enter a second matrix. \nEnter C to only use one matrix: ";
            readSTD()
        )
    in
        if c = "c\n" orelse c = "C\n" then
            oneMatrix(m)
        else
            twoMatrix(m, parseMatrix(c))
    end
    handle Fail s => (
        print("\nSomething went wrong while trying to parse the input! Please try again.\n"); 
        secondMatrix(m)
    )

(*
    start
    TYPE:   unit -> unit
    PRE:    None
    POST:   None
    SIDE-EFFECTS: Reads from stdIn and prints to the console.
*)
fun start() =
    let 
        val m = (
            print "Please enter a matrix: ";
            parseMatrix(readSTD())
        )
    in
        print("\nResult: \n" ^ secondMatrix(m) ^ "\n")
    end
    handle 
        Fail s => (
            print("\nSomething went wrong while trying to parse the input! Please try again.\n"); 
            start()
        )
      | Match  => (
            print("\nSomething went wrong in the calculation! Make sure your matrix is a square matrix if needed.\n");
            start()
        ) 
        
(*
    test
    TYPE:   unit -> unit
    PRE:    None
    POST:   None
    SIDE-EFFECTS: Performs unit tests on the two datatypes fractal and matrix, and prints the results to the console.
*)
fun test() = (
        fractalTest();
        matrixTest()
    )