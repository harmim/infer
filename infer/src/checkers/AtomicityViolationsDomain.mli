(** Detection of atomicity violations domain interface. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd

module F = Format

(* ****************************** Initialisation **************************** *)

val initialise : unit -> unit
(** The initialisation of the abstract domain. *)

(* ****************************** Astate ************************************ *)

type t
(** An abstract state of a function. *)

val initial : t
(** An initial abstract state of an analysed function. *)

val pp : F.formatter -> t -> unit
(** A pretty printer of an abstract state. *)

val update_astate_on_function_call : t -> string -> Location.t -> t
(** Updates an abstract state on a function call. *)

val update_astate_on_lock : t -> AccessPath.t option -> t
(** Updates an abstract state on a lock call. *)

val update_astate_on_unlock : t -> AccessPath.t option -> t
(** Updates an abstract state on an unlock call. *)

(* ****************************** Summary *********************************** *)

type summary
(** A summary of a function. *)

val pp_summary : F.formatter -> summary -> unit
(** A pretty printer of a summary. *)

val update_astate_with_summary : t -> summary -> Location.t -> t
(** Updates an abstract state on a function call with its summary. *)

val astate_to_summary : t -> summary
(** Converts an abstract state to a summary. *)

val report_atomicity_violations : t -> (Location.t -> string -> unit) -> unit
(** Reports atomicity violations from an abstract state using reporting
    function. *)

(* ****************************** Operators ********************************* *)

val leq : lhs:t -> rhs:t -> bool
(** A comparison operator of abstract states. *)

val join : t -> t -> t
(** A join operator of abstract states. *)

val widen : prev:t -> next:t -> num_iters:int -> t
(** A widen operator of abstract states. *)
