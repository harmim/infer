(** Detection of atomicity violations interface. *)

open! IStd

val analyse_procedure : Callbacks.proc_callback_args -> Summary.t
(** Atomicity violations detection entry point. Produces a summary for
    a given function. Should be invoked for each function in the analysed
    program. Reports atomicity violations. *)
