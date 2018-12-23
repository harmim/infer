(** Atomicity detection interface. *)

open! IStd

val checker : Callbacks.proc_callback_args -> Summary.t
(** Atomicity detection entry point. Produces summary for given function.
    Should be invoked for every function in analyzed program. *)

val reporting : Callbacks.cluster_callback_args -> unit
(** Should be invoked after atomicity detection of all functions
    in analyzed program. Prints atomicity sequences from summaries
    from all analyzed functions. *)
