(* File Ast.ml *)
type arithmetic =
  |Int of int
  |Var of string
  |Neg of arithmetic
  |Add of arithmetic * arithmetic
  |Mul of arithmetic * arithmetic
  |Div of arithmetic * arithmetic
  |Pow of arithmetic * arithmetic
  |Mod of arithmetic * arithmetic

type boolean =
  |True
  |False
  |Not of boolean
  |And of boolean * boolean
  |Or of boolean * boolean
  |Leq of arithmetic * arithmetic
  |Eq of arithmetic * arithmetic
  |Forall of string list * boolean

type command = 
  |Skip
  |Let of string * arithmetic * command
  |If of boolean * command * command
  |While of boolean * command
  |Seq of command * command