open Papote.Program
open Papote.Caus_sem
open Causality_tracing

let with_loc i x = {obj=x;loc={line=i;column=0}}

let assign i a c cont =
  Assign (with_loc i (G a, Const c), cont)

let store i r a cont =
  Store (with_loc i (R r, G a), cont)

let example =
  [ store 0 "r" "a" Unit
  ; assign 1 "a" 2 (assign 1 "a" 3 Unit) ]

let () =
  let es = trace sc example in
  let dot = ES.to_dot (fun x -> [Dot.Attribute.label (string_of_label (Event.label x))]) es in
  Dot.view dot
