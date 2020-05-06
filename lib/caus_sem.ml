open Causality.Monad
open Causality
open Causality_tracing
open Program

module GM = Map.Make(struct type t = global let compare = compare end)
module RM = Map.Make(struct type t = register let compare = compare end)

type buffer = (global * (location * int)) list

type label = Write of location * string * int | Read of location * string * int

let string_of_global (G g) = g

let string_of_label x =
  match x with
  | Write (_,a,i) -> "Wr " ^ a ^ " " ^ string_of_int i
  | Read (_,a,i) -> "Re " ^ a ^ " " ^ string_of_int i

module Tracing = Tracing.Make(struct type nonrec label = label end)

let op_of_binop b =
  match b with
  | Sub -> ( - )
  | Add ->  ( + )
  | Mul -> ( * )
  | Div -> ( / )

let iarith rm (a:arith) : int t =
  let rec aux = function
  | Const i -> return i
  | Reg r -> return (RM.find r rm)
  | Binop (b,l,r) -> aux l >>= fun l -> aux r >|= fun r -> op_of_binop b l r
  in aux a

let write_buffer gm buffer =
  let aux (g,(loc,v)) =
    let act = Tracing.emit_in_view (Write (loc,string_of_global g,v)) in
    Cell.set ~act (GM.find g gm) v in
  List.fold_right (fun x acc -> acc >> aux x) buffer (return ())

let set_cell analyse_buff loc g v gm buffer =
  let buffer = (g,(loc,v))::buffer in
  if analyse_buff buffer
  then write_buffer gm buffer >> return []
  else return buffer

let get_cell loc g gm buffer =
  match List.assoc_opt g buffer with
  | Some x -> return (snd x)
  | None ->
     let act v = Tracing.emit_in_view (Read (loc,string_of_global g,v)) in
     Cell.get ~act (GM.find g gm)

let ithread (analyse_buf : buffer -> bool) gm (t:thread) : unit t =
  let rec aux (buffer : buffer) rm = function
  | Unit ->
     return ()
  | IfThenElse (i,t,e) ->
     iarith rm i.obj >>= (fun i -> if i = 0 then aux buffer rm t else aux buffer rm e)
  | Assign ((x,t)) ->
     let g,e = x.obj in
     iarith rm e >>= fun e ->
     set_cell analyse_buf x.loc g e gm buffer >>= fun buffer ->
     aux buffer rm t
  | Store (x,t) ->
     let r,g = x.obj in
     get_cell x.loc g gm buffer >>= fun e ->
     aux buffer (RM.add r e rm) t
  in aux [] RM.empty t

let iprogram analyse_buf (p:program) : unit t =
  GS.fold (fun g acc -> acc >>= fun acc -> Cell.create 0 >|= fun c -> GM.add g c acc)
    (globals_of_program p) (return GM.empty) >>= fun gm ->
  spawn_list_map p (ithread analyse_buf gm)

let trace analyse_buf (p:program) : label ES.t =
  Tracing.get_es (Run.comp (Tracing.init >> iprogram analyse_buf p))

let sc _ = true
