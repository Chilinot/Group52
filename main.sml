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
  

fun secondMatrix(m) = 
    let
    in
    end

fun start() =
    let 
        val s = (
            print "Please enter a matrix: ";
            case parseMatrix(readLine()) of
                SOME m => m
              | NONE   => (print "You need to enter something!"; start())
        )
    in
        secondMatrix(m)
    end
    























