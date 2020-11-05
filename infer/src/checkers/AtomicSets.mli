(* Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd

(** Detection of atomic sets interface. *)

val analyse_procedure : Callbacks.proc_callback_t
(** An atomic sets detection entry point. Produces a summary for a given function. Should be invoked
    for each function in the analysed program. *)

val print_atomic_sets : Callbacks.file_callback_t
(** Should be invoked after the atomic sets detection of all functions in the analysed program.
    Prints atomic sets from summaries from all analysed functions. *)
