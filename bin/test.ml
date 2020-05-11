open Papote.Program
open Papote.Caus_sem
open Causality_tracing

let with_loc i x = {obj=x;loc={line=i;column=0}}

let store i a c cont =
  Store (with_loc i (G a, Const c), cont)

let load i r a cont =
  Load (with_loc i (R r, G a), cont)

let example =
  [ load 0 "r" "a" Unit
  ; store 1 "a" 2 (store 1 "a" 3 Unit) ]

let () =
  let es = trace rand example in
  let dot = ES.to_dot (fun x -> [Dot.Attribute.label (string_of_label (Event.label x))]) es in
  Dot.view dot
