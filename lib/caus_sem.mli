open Program
open Causality

type label = Write of location * string * int | Read of location * string * int
val string_of_label : label -> string

(** A buffer is a list of global variables together with their assignment location and their values. *)
type buffer = (global * (location * int)) list

(** The first argument tells at each step if we commit a value. *)
val iprogram : (buffer -> bool Monad.t) -> program -> unit Monad.t
val trace : (buffer -> bool Monad.t) -> program -> label Causality_tracing.ES.t

(** Always commit values. *)
val sc : buffer -> bool Monad.t

(** Commit values randomly. *)
val rand : buffer -> bool Monad.t
