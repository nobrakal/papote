type location = {line:int; column:int}
type 'a located = {obj:'a; loc: location}

type register = R of string
type global = G of string

type binop = Mul | Div | Add | Sub

type arith =
  | Const of int
  | Reg   of register
  | Binop of binop * arith * arith

type thread =
  | Assign of (global * arith) located * thread
  | Store of (register * global) located * thread
  | IfThenElse of arith located * thread * thread
  | Unit

type program = thread list

module GS : Set.S with type elt = global

val globals_of_program : program -> GS.t
