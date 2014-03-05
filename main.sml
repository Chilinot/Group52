use "matrix.sml";

(* readLine ()
   TYPE: unit -> string
   PRE: true
   POST: the current line of input from stdIn
   SIDE-EFFECTS: reads from stdIn
 *)
fun readLine () = valOf(TextIO.inputLine TextIO.stdIn)

fun twoMatrix(m1, m2) = (
        print(
            "\nWhat would you like to calculate?\n" ^
            "1. Addition \n" ^
            "2. Subtraction \n" ^
            "3. Multiplication \n" ^
            "CHOICE: "
        );
        case readLine() of
            "1\n" => matrixToString(mAdd(m1, m2))
          | "2\n" => matrixToString(mSub(m1, m2))
          | "3\n" => matrixToString(mMult(m1, m2))
          | _     => (print("Incorrect choice! Please try again.\n"); twoMatrix(m1, m2))
    )

fun oneMatrix(m) = (
        print(
            "\nYou have only entered one matrix. \nWhat would you like to calculate?\n" ^
            "1. Inverse \n" ^
            "2. Determinant \n" ^
            "3. Adjoint \n" ^
            "4. Cofactor matrix \n" ^
            "CHOICE: "
        );
        case readLine() of
            "1\n" => matrixToString(mInv(m))
          | "2\n" => fracToString(mDet(m))
          | "3\n" => matrixToString(mAdjoint(m))
          | "4\n" => matrixToString(mCofactor(m))
          | _     => (print("Incorrect choice! Please try again.\n"); oneMatrix(m))
    )

fun secondMatrix(m) = 
    let
        val c = (
            print "Please enter a second matrix. \nEnter C to only use one matrix: ";
            readLine()
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

fun start() =
    let 
        val m = (
            print "Please enter a matrix: ";
            parseMatrix(readLine())
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























