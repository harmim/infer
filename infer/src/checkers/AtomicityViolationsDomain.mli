(* Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd
module F = Format

(** Detection of atomicity violations domain interface. *)

include AbstractDomain.S

(* ************************************ Initialisation ****************************************** *)

val initialise : unit -> unit
(** The initialisation of the abstract domain. *)

(* ************************************ Astate ************************************************** *)

val initial : t
(** An initial abstract state of an analysed function. *)

val apply_call : t -> string -> Location.t -> t
(** Updates an abstract state on a function call. *)

val apply_lock : ?ap:AccessPath.t option -> t -> t
(** Updates an abstract state on a lock call. *)

val apply_unlock : ?ap:AccessPath.t option -> t -> t
(** Updates an abstract state on an unlock call. *)

(* ************************************ Summary ************************************************* *)

(** A summary of a function. *)
type summary

val pp_summary : F.formatter -> summary -> unit
(** A pretty printer of a summary. *)

val apply_summary : t -> summary -> Location.t -> t
(** Updates an abstract state on a function call with its summary. *)

val astate_to_summary : t -> summary
(** Converts an abstract state to a summary. *)

val report_atomicity_violations : t -> f:(Location.t -> msg:string -> unit) -> unit
(** Reports atomicity violations from an abstract state using reporting function. *)
