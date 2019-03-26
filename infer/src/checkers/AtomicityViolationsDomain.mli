(** Detection of atomicity violations domain interface. *)

open! IStd

module F = Format

(* ****************************** Astate ************************************ *)

type t
(** The abstract state of a function. *)

val initial : t
(** The initial abstract state of each analyzed function. *)

val pp : F.formatter -> t -> unit
(** Pretty printer of the abstract state. *)

(* ****************************** Summary *********************************** *)

type summary
(** The summary of a function. *)

val initialSummary : summary

val pp_summary : F.formatter -> summary -> unit
(** Pretty printer of the summary. *)

(* ****************************** Operators ********************************* *)

val ( <= ) : lhs:t -> rhs:t -> bool
(** The comparison operator of abstract states. *)

val join : t -> t -> t
(** The join operator of abstract states. *)

val widen : prev:t -> next:t -> num_iters:int -> t
(** The widen operator of abstract states. *)
