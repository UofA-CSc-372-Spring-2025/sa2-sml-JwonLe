(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Juwon Lee *)
(* Time spent on HW6: 3 days
*)

(* Collaborators and references: ChatGPT, StackOverflow
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)

fun mynull []       = true
  | mynull _   = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true

val () =
    Unit.checkExpectWith Bool.toString "mynull [0] should be false"
    (fn () => mynull [0])
    false

val () =
    Unit.checkExpectWith Bool.toString "mynull [0,1,2] should be false"
    (fn () => mynull [0,1,2])
    false


(**** Problem B ****)
fun firstVowel [] = false
  | firstVowel (x ::xs) =
    case x of
      #"a" => true
      | #"e" => true
      | #"i" => true
      | #"o" => true
      | #"u" => true
      | _ => false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

val () =
    Unit.checkAssert "Test for firstVowel 'e'" (fn () => firstVowel [#"e"])
  
val () =
    Unit.checkExpectWith Bool.toString "firstVowel '' should be false"
    (fn () => firstVowel [])
    false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'bb' should be false"
    (fn () => firstVowel [#"b", #"b"])
    false


(**** Problem C ****)
fun reverse xs = 
  foldl (fn (x, acc) => x :: acc) [] xs

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1] should be [1]"
  (fn () => reverse [1])
  [1]

val () =
  Unit.checkExpectWith (Unit.listString Char.toString) 
  "reverse [a, b, c] should be [c, b, a]"
  (fn () => reverse [#"a", #"b", #"c"])
  [#"c", #"b", #"a"]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "reverse [] should be []"
  (fn () => reverse [] : int list)
  []


(**** Problem D ****)
fun minlist [] = raise Match
| minlist(x :: xs) = foldl (fn (x, acc) => if x < acc then x else acc) x xs

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,-3,4,0] should be -3"
  (fn () => minlist [1,2,~3,4,0])
  ~3

(**** Problem E ****)
exception Mismatch

fun zip [] [] = []
  | zip (x ::xs) (y :: ys) = (x, y) :: zip xs ys
  | zip _ _ = raise Mismatch

val () =
  Unit.checkExpectWith (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip [] [] should be []"
  (fn () => zip [] [] : (int * int) list)
  []

val () =
  Unit.checkExnWith (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip [] [1] should raise an exception"
  (fn () => zip [] [1])

val () =
  Unit.checkExnWith (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip [1,2,3] [] should raise an exception"
  (fn () => zip [1,2,3] [])

val () =
  Unit.checkExpectWith (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip [1,2,3] [1,2,3] should be [(1,1),(2,2),(3,3)]"
  (fn () => zip [1,2,3] [1,2,3])
  [(1,1),(2,2),(3,3)]

val () =
  Unit.checkExpectWith (Unit.listString (Unit.pairString Char.toString Char.toString))
  "zip [a,b,c] [a,b,c] should be [(a,a),(b,b),(c,c)]"
  (fn () => zip [#"a",#"b",#"c"] [#"a",#"b",#"c"])
  [(#"a",#"a"),(#"b",#"b"),(#"c",#"c")]


(**** Problem F ****)
fun concat xs =  foldl (fn (x, acc) => acc @ x) [] xs

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[1], [2, 3, 4], [], [5, 6]] should be [1, 2, 3, 4, 5, 6]"
  (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
  [1, 2, 3, 4, 5, 6]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [] should be []"
  (fn () => concat [])
  []

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[]] should be []"
  (fn () => concat [[]])
  []

val () =
  Unit.checkExpectWith (Unit.listString Char.toString)
  "concat [[a], [b,c], [], [d]] should be [a,b,c,d]"
  (fn () => concat [[#"a"], [#"b",#"c"], [], [#"d"]])
  [#"a", #"b", #"c",#"d"]


(**** Problem G ****)
fun isDigit x = 
  case x of
    #"0" => true
    | #"1" => true
    | #"2" => true
    | #"3" => true
    | #"4" => true
    | #"5" => true
    | #"6" => true
    | #"7" => true
    | #"8" => true
    | #"9" => true
    | #"0" => true
    | _ => false

val () =
 Unit.checkAssert "isDigit 0 should be true" (fn () => isDigit #"0");

val () =
 Unit.checkAssert "isDigit 8 should be true" (fn () => isDigit #"8");

val () =
 Unit.checkAssert "isDigit 6 should be true" (fn () => isDigit #"6");

val () =
  Unit.checkExpectWith Bool.toString
  "isDigit ~ should be false"
  (fn () => isDigit #"~")
  false

val () =
  Unit.checkExpectWith Bool.toString
  "isDigit a should be false"
  (fn () => isDigit #"a")
  false


(**** Problem H ****)
fun isAlpha c = 
  let 
    val n = Char.ord c
  in
    (n >= 65 andalso n <= 90) orelse (n >= 97 andalso n <= 122)
  end;

val () =
 Unit.checkAssert "isAlpha a should be true" (fn () => isAlpha #"a");

val () = 
  Unit.checkAssert "isAlpha Z should be true" (fn () => isAlpha #"Z");

val () =
  Unit.checkExpectWith Bool.toString
  "isAlpha 0 should be false"
  (fn () => isAlpha #"0")
  false

(**** Problem I ****)
(*
fun svgCircle (cx, cy, r, fill) = "NOT IMPLEMENTED YET"

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";
*)
(**** Problem J ****)
(*
fun partition p (x :: xs) = ([],[])

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);
*)

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
