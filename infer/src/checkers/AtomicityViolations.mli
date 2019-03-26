(** Detection of atomicity violations interface. *)

open! IStd

val analyze_procedure : Callbacks.proc_callback_args -> Summary.t
(** The atomicity violations detection entry point. Produces a summary for the
    given function. Should be invoked for each function in an analyzed
    program. Reports atomicity violations. *)
