open Core.Ast

(* 1. Simple assignment *)
(* Program: x := x + 1 *)
let prog1 = Assign ("x", Add (Var "x", Int 1))

(* Precondition: x ≥ 0 *)
let pre1 = LeqF (Int 0, Var "x")

(* Postcondition: x ≥ 1 *)
let post1 = LeqF (Int 1, Var "x")

(* 2. If statement *)
(* Program:
   if x ≤ 10 then y := 1 else y := 0
*)
let prog2 =
  If (
    Leq (Var "x", Int 10),
    Assign ("y", Int 1),
    Assign ("y", Int 0)
  )

(* Precondition: true *)
let pre2 = TrueF

(* Postcondition: y = 0 ∨ y = 1 *)
let post2 =
  OrF (
    EqF (Var "y", Int 0),
    EqF (Var "y", Int 1)
  )

(* 3. Sequence of two assignments *)
(* Program:
   x := x + 1;
   y := x * 2
*)
let prog3 =
  Seq (
    Assign ("x", Add (Var "x", Int 1)),
    Assign ("y", Mul (Var "x", Int 2))
  )

(* Precondition: x ≥ 0 *)
let pre3 = LeqF (Int 0, Var "x")

(* Postcondition: y ≥ 2 *)
let post3 = LeqF (Int 2, Var "y")

(* 4. While loop with invariant *)
(* Program:
   while x ≤ 10 invariant 0 ≤ x do
     x := x + 1
   done
*)
let prog4 =
  While (
    Leq (Var "x", Int 10),
    AndF (LeqF (Int 0, Var "x"), LeqF (Var "x", Int 11)),  (* loop invariant: 0 ≤ x ≤ 11 *)
    Assign ("x", Add (Var "x", Int 1))
  )

(* Precondition: 0 ≤ x *)
let pre4 = LeqF (Int 0, Var "x")

(* Postcondition: x = 11 *)
let post4 = EqF (Var "x", Int 11)

(* 5. Nested If and While *)
(* Program:
   if x ≤ 5 then
     while y ≤ 3 invariant y ≥ 0 do
       y := y + 1
     done
   else
     y := 0
*)
let prog5 =
  If (
    Leq (Var "x", Int 5),
    While (
      Leq (Var "y", Int 3),
      LeqF (Int 0, Var "y"),
      Assign ("y", Add (Var "y", Int 1))
    ),
    Assign ("y", Int 0)
  )

(* Precondition: y ≥ 0 *)
let pre5 = LeqF (Int 0, Var "y")

(* Postcondition: y ≥ 0 *)
let post5 = LeqF (Int 0, Var "y")