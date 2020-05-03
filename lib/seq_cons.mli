open Program

type label = Write of location * string * int | Read of location * string * int
val string_of_label : label -> string

val iprogram : program -> unit Causality.Monad.t
val trace : program -> label Causality_tracing.ES.t
