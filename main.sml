use "matrix.sml";

(* readLine ()
   TYPE: unit -> string
   PRE: true
   POST: the current line of input from stdIn
   SIDE-EFFECTS: reads from stdIn
 *)
fun readLine () = valOf(TextIO.inputLine TextIO.stdIn);

fun secondMatrix(m) = 
    let
        val c = (
            print "Please enter a second matrix: ";
            readLine()
        )
    in
        if c = "c\n" then
            oneMatrix(m)
        else
            twoMatrix(m, parseMatrix(c))
    end
    handle Fail s => (
        print("\nSomething went wrong while trying to parse the input! Please try again.\n"); 
        start()
    )

fun start() =
    let 
        val m = (
            print "Please enter a matrix: ";
            parseMatrix(readLine())
        )
    in
        secondMatrix(m)
    end
    handle Fail s => (
        print("\nSomething went wrong while trying to parse the input! Please try again.\n"); 
        start()
    )
    























