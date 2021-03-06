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
  | Store of (global * arith) located * thread
  | Load of (register * global) located * thread
  | IfThenElse of arith located * thread * thread
  | Mfence of thread
  | Unit

type program = thread list

module GS = Set.Make(struct type t = global let compare = compare end)

let globals_of_program p =
  let rec thread acc = function
    | Store (x,t) -> GS.add (fst x.obj) (thread acc t)
    | Load (x,t) -> GS.add (snd x.obj) (thread acc t)
    | IfThenElse (_,t1,t2) -> thread (thread acc t1) t2
    | Mfence t -> thread acc t
    | Unit -> acc
  in
  List.fold_left thread GS.empty p
