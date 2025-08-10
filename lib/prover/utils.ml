open Core.Ast

let rec form_of_bool b = match b with
  | True -> TrueF
  | False -> FalseF
  | Not b -> NotF (form_of_bool b)
  | And (b1, b2) -> AndF (form_of_bool b1, form_of_bool b2)
  | Or (b1, b2) -> OrF (form_of_bool b1, form_of_bool b2)
  | Eq (a1, a2) -> EqF (a1, a2)
  | Leq (a1, a2) -> LeqF (a1, a2)

let rec replace_arith var arith a1 = match a1 with
  | Int i -> Int i
  | Var v when v = var -> arith
  | Var v -> Var v
  | Neg a -> Neg (replace_arith var arith a)
  | Add (a1, a2) -> Add (replace_arith var arith a1, replace_arith var arith a2)
  | Mul (a1, a2) -> Mul (replace_arith var arith a1, replace_arith var arith a2)
  | Div (a1, a2) -> Div (replace_arith var arith a1, replace_arith var arith a2)
  | Pow (a1, a2) -> Pow (replace_arith var arith a1, replace_arith var arith a2)
  | Mod (a1, a2) -> Mod (replace_arith var arith a1, replace_arith var arith a2)

let rec replace_formula var arith formula = match formula with
  |TrueF -> TrueF
  |FalseF -> FalseF
  |NotF f -> NotF (replace_formula var arith f)
  |AndF (f1, f2) -> AndF (replace_formula var arith f1, replace_formula var arith f2)
  |OrF (f1, f2) -> OrF (replace_formula var arith f1, replace_formula var arith f2)
  |EqF (a1, a2) -> EqF (replace_arith var arith a1, replace_arith var arith a2)
  |LeqF (a1, a2) -> LeqF (replace_arith var arith a1, replace_arith var arith a2)
  |ImplyF (f1, f2) -> ImplyF (replace_formula var arith f1, replace_formula var arith f2)