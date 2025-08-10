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
  |Eq of arithmetic * arithmetic
  |Leq of arithmetic * arithmetic

type formula =
  |TrueF
  |FalseF
  |NotF of formula
  |AndF of formula * formula
  |OrF of formula * formula
  |ImplyF of formula * formula
  |EqF of arithmetic * arithmetic
  |LeqF of arithmetic * arithmetic

type command = 
  |Skip
  |Assign of string * arithmetic
  |If of boolean * command * command
  |While of boolean * formula * command
  |Seq of command * command