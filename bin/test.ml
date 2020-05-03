open Papote.Program
open Papote.Seq_cons
open Causality_tracing

let with_loc i x = {obj=x;loc={line=i;column=0}}

let assign i a c cont =
  Assign (with_loc i (G a, Const c), cont)

let store i r a cont =
  Store (with_loc i (R r, G a), cont)

let example =
  [ store 0 "r" "a" Unit
  ; assign 1 "a" 2 Unit
  ; assign 2 "a" 1 Unit ]

let () =
  let es = Papote.Seq_cons.trace example in
  let dot = ES.to_dot (fun x -> [Causality_tracing.Dot.Attribute.label (string_of_label (Event.label x))]) es in
  Causality_tracing.Dot.to_file dot "test.dot"
