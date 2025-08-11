open Ast

let rec string_of_arith = function
  | Int i -> string_of_int i
  | Var s -> s
  | Neg a -> "-" ^ string_of_arith a
  | Add (a1, a2) -> "(" ^ string_of_arith a1 ^ " + " ^ string_of_arith a2 ^ ")"
  | Mul (a1, a2) -> "(" ^ string_of_arith a1 ^ " * " ^ string_of_arith a2 ^ ")"
  | Div (a1, a2) -> "(" ^ string_of_arith a1 ^ " / " ^ string_of_arith a2 ^ ")"
  | Pow (a1, a2) -> "(" ^ string_of_arith a1 ^ " ^ " ^ string_of_arith a2 ^ ")"
  | Mod (a1, a2) -> "(" ^ string_of_arith a1 ^ " % " ^ string_of_arith a2 ^ ")"

let rec string_of_bool = function
  | True -> "true"
  | False -> "false"
  | Not b -> "not " ^ string_of_bool b
  | And (b1, b2) -> "(" ^ string_of_bool b1 ^ " && " ^ string_of_bool b2 ^ ")"
  | Or (b1, b2) -> "(" ^ string_of_bool b1 ^ " || " ^ string_of_bool b2 ^ ")"
  | Eq (a1, a2) -> "(" ^ string_of_arith a1 ^ " = " ^ string_of_arith a2 ^ ")"
  | Leq (a1, a2) -> "(" ^ string_of_arith a1 ^ " <= " ^ string_of_arith a2 ^ ")"

let rec string_of_formula = function
  | TrueF -> "true"
  | FalseF -> "false"
  | NotF f -> "not " ^ string_of_formula f
  | AndF (f1, f2) -> "(" ^ string_of_formula f1 ^ " && " ^ string_of_formula f2 ^ ")"
  | OrF (f1, f2) -> "(" ^ string_of_formula f1 ^ " || " ^ string_of_formula f2 ^ ")"
  | ImplyF (f1, f2) -> "(" ^ string_of_formula f1 ^ " => " ^ string_of_formula f2 ^ ")"
  | EqF (a1, a2) -> "(" ^ string_of_arith a1 ^ " = " ^ string_of_arith a2 ^ ")"
  | LeqF (a1, a2) -> "(" ^ string_of_arith a1 ^ " <= " ^ string_of_arith a2 ^ ")"

let rec string_of_command = function
  | Skip -> "skip"
  | Assign (x, e) -> x ^ " := " ^ string_of_arith e
  | Seq (c1, c2) -> "(" ^ string_of_command c1 ^ "; " ^ string_of_command c2 ^ ")"
  | If (cond, then_branch, else_branch) ->
    "if " ^ string_of_bool cond ^ " then " ^ string_of_command then_branch ^
    " else " ^ string_of_command else_branch
  | While (cond, _, body) ->
    "while " ^ string_of_bool cond ^ " do " ^ string_of_command body ^ " done"

let string_of_program prog =
  string_of_formula prog.precondition ^ "\n" ^ string_of_command prog.body ^ "\n" ^ string_of_formula prog.postcondition