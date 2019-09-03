(** Detection of atomic sets domain interface. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd

module F = Format
module OC = Out_channel

(* ****************************** Astate ************************************ *)

type t
(** Abstract state of a function. *)

val initial : t
(** Initial abstract state of an analysed function. *)

val pp : F.formatter -> t -> unit
(** Pretty printer of an abstract state. *)

val update_astate_on_function_call : t -> string -> t
(** Updates an abstract state on a function call. *)

val update_astate_on_lock : t -> t
(** Updates an abstract state on a lock call. *)

val update_astate_on_unlock : t -> t
(** Updates an abstract state on an unlock call. *)

val update_astate_at_the_end_of_function : t -> t
(** Updates an abstract state at the end of a function. *)

(* ****************************** Summary *********************************** *)

type summary
(** Summary of a function. *)

val pp_summary : F.formatter -> summary -> unit
(** Pretty printer of a summary. *)

val update_astate_on_function_call_with_summary : t -> summary -> t
(** Updates an abstract state on a function call with its summary. *)

val convert_astate_to_summary : t -> summary
(** Converts an abstract state to a summary. *)

val print_atomic_sets : OC.t -> string -> summary -> unit
(** Prints atomic sets from a given summary and a function name to a given
    output channel. *)

(* ****************************** Operators ********************************* *)

val ( <= ) : lhs:t -> rhs:t -> bool
(** Comparison operator of abstract states. *)

val join : t -> t -> t
(** Join operator of abstract states. *)

val widen : prev:t -> next:t -> num_iters:int -> t
(** Widen operator of abstract states. *)
