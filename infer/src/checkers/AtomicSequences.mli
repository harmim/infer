(** Detection of atomic sequences interface. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd

val analyse_procedure : Callbacks.proc_callback_args -> Summary.t
(** Atomic sequences detection entry point. Produces a summary for a given
    function. Should be invoked for each function in the analysed program. *)

val print_atomic_sequences : Callbacks.cluster_callback_args -> unit
(** Should be invoked after the atomic sequences detection of all functions
    in the analysed program. Prints atomic sequences from summaries
    from all analszed functions. *)
