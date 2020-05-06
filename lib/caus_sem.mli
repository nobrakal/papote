open Program

type label = Write of location * string * int | Read of location * string * int
val string_of_label : label -> string

(** A buffer is a list of global variables together with their assignment location and their values. *)
type buffer = (global * (location * int)) list

val iprogram : (buffer -> bool) -> program -> unit Causality.Monad.t
val trace : (buffer -> bool) -> program -> label Causality_tracing.ES.t

val sc : buffer -> bool
