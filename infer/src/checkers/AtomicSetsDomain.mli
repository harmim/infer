(* Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd
module F = Format

include AbstractDomain.S

(** Detection of atomic sets domain interface. *)

(* ************************************ Astate ************************************************** *)

val initial : t
(** An initial abstract state of an analysed function. *)

val apply_call : t -> string -> t
(** Updates an abstract state on a function call. *)

val apply_lock : ?ap:AccessPath.t option -> t -> t
(** Updates an abstract state on a lock call. *)

val apply_unlock : ?ap:AccessPath.t option -> t -> t
(** Updates an abstract state on an unlock call. *)

val update_at_the_end_of_function : t -> t
(** Updates an abstract state at the end of a function. *)

(* ************************************ Summary ************************************************* *)

(** A summary of a function. *)
type summary

val pp_summary : F.formatter -> summary -> unit
(** A pretty printer of a summary. *)

val apply_summary : t -> summary -> t
(** Updates an abstract state on a function call with its summary. *)

val astate_to_summary : t -> summary
(** Converts an abstract state to a summary. *)

val print_atomic_sets : Out_channel.t -> f_name:string -> summary -> unit
(** Prints atomic sets from a given summary and a function name to a given output channel. *)
