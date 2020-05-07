open Program

type label = Write of location * string * int | Read of location * string * int
val string_of_label : label -> string

(** A buffer is a list of global variables together with their assignment location and their values. *)
type buffer = (global * (location * int)) list

(** The first argument tells the number of cells of a buffer to commit. *)
val iprogram : (buffer -> int) -> program -> unit Causality.Monad.t
val trace : (buffer -> int) -> program -> label Causality_tracing.ES.t

val sc : buffer -> int
