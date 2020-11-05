(* Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd

(** Detection of atomicity violations interface. *)

val analyse_procedure : Callbacks.proc_callback_t
(** An atomicity violations detection entry point. Produces a summary for a given function. Should
    be invoked for each function in the analysed program. Reports atomicity violations. *)
