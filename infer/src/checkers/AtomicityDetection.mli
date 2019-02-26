(** Atomicity detection interface. *)

open! IStd

val checker : Callbacks.proc_callback_args -> Summary.t
(** The atomicity detection entry point. Produces a summary for the given
    function. Should be invoked for each function in an analyzed program. *)

val reporting : Callbacks.cluster_callback_args -> unit
(** Should be invoked after the atomicity detection of all functions
    in an analyzed program. Prints atomicity sequences from summaries
    from all analyzed functions. *)
