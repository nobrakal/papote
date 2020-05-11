open Papote.Program
open Papote.Caus_sem
open Causality_tracing

let with_loc i x = {obj=x;loc={line=i;column=0}}

let store i a c cont =
  Store (with_loc i (G a, Const c), cont)

let load i r a cont =
  Load (with_loc i (R r, G a), cont)

let store_buffering =
  [ store 0 "x" 1 (load 0 "r" "y" Unit)
  ; store 1 "y" 1 (load 1 "r" "x" Unit) ]

let read_own_write_early =
  [ store 0 "x" 1 (load 0 "r" "x" (load 0 "r" "y" Unit))
  ; store 1 "y" 1 (load 1 "r" "y" (load 1 "r" "x" Unit)) ]

let () =
  let es = trace sc read_own_write_early in
  let dot = ES.to_dot (fun x -> [Dot.Attribute.label (string_of_label (Event.label x))]) es in
  Dot.view dot
