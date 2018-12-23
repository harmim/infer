(** Atomicity detection domain interface. *)

open! IStd
module F = Format

(* ****************************** Functions ********************************* *)

val is_lock : string -> bool
(** Checks whether given function is lock. *)

val is_unlock : string -> bool
(** Checks whether given function is unlock. *)

(* ****************************** Astate ************************************ *)

type t
(** Abstract state of function. *)

val initial : t
(** Initial abstract state of each analyzed function. *)

val pp : F.formatter -> t -> unit
(** Pretty printer of abstract state. *)

val update_astate_on_function_call : t -> string -> t
(** Updates abstract state on function call. *)

val update_astate_on_lock : t -> t
(** Updates abstract state on lock call. *)

val update_astate_on_unlock : t -> t
(** Updates abstract state on unlock call. *)

val update_astate_at_the_end_of_function : t -> t
(** Updates abstract state at the end of function. *)

(* ****************************** Summary *********************************** *)

type summary
(** Summary of function. *)

val pp_summary : F.formatter -> summary -> unit
(** Pretty printer of summary. *)

val update_astate_on_function_call_with_summary : t -> summary -> t
(** Updates abstract state on function call with with its summary. *)

val convert_astate_to_summary : t -> summary
(** Converts abstract state to summary. *)

(* ****************************** Operators ********************************* *)

val ( <= ) : lhs:t -> rhs:t -> bool
(** Comparison operator of abstract states. *)

val join : t -> t -> t
(** Join operator of abstract states.  *)

val widen : prev:t -> next:t -> num_iters:int -> t
(** Widen operator of abstract states.  *)
