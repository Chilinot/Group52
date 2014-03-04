use "matrix.sml";

(* readLine ()
   TYPE: unit -> string
   PRE: true
   POST: the current line of input from stdIn
   SIDE-EFFECTS: reads from stdIn
 *)
fun readLine () = valOf(TextIO.inputLine TextIO.stdIn);

(* readNumber ()
   TYPE: unit -> real
   PRE: true
   POST: a real number read from stdIn
   SIDE-EFFECTS: prints to and reads from stdIn
 *)
fun readNumber () =
  (
    print "Enter a number: ";
    case Int.fromString (readLine ()) of
      SOME r => r
    | NONE => (print "Invalid number!\n"; readNumber ())
  );
  
fun menu() =
    let
        val m = print(
            "1. Enter a matrix.\n" ^ 
            "2. Enter a second matrix.\n" ^ 
            "3. Calculate inverse \t of first.\n" ^ 
            "4.    ...    determinant \t ... .\n" ^ 
            "5.    ...    adjugate \t ... .\n" ^ 
            "6.    ...    cofactor matrix \t ... .\n"
        )
    in
        case readNumber() of
            1 => 
    end



























