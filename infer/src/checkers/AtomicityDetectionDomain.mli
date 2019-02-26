(** Atomicity detection domain interface. *)

open! IStd
module F = Format
module OC = Out_channel

(* ****************************** Functions ********************************* *)

val is_lock : string -> bool
(** Checks whether the given function is a lock. *)

val is_unlock : string -> bool
(** Checks whether the given function is an unlock. *)

(* ****************************** Astate ************************************ *)

type t
(** The abstract state of a function. *)

val initial : t
(** The initial abstract state of each analyzed function. *)

val pp : F.formatter -> t -> unit
(** Pretty printer of the abstract state. *)

val update_astate_on_function_call : t -> string -> t
(** Updates the abstract state on the function call. *)

val update_astate_on_lock : t -> t
(** Updates the abstract state on a lock call. *)

val update_astate_on_unlock : t -> t
(** Updates the abstract state on an unlock call. *)

val update_astate_at_the_end_of_function : t -> t
(** Updates the abstract state at the end of a function. *)

(* ****************************** Summary *********************************** *)

type summary
(** The summary of a function. *)

val pp_summary : F.formatter -> summary -> unit
(** Pretty printer of the summary. *)

val update_astate_on_function_call_with_summary : t -> summary -> t
(** Updates the abstract state on the function call with its summary. *)

val convert_astate_to_summary : t -> summary
(** Converts the abstract state to a summary. *)

(* ****************************** Reporting ********************************* *)

val report : OC.t -> string -> summary -> unit
(** Prints atomicity sequences from the given summary and the function name
    to the given output channel. *)

(* ****************************** Operators ********************************* *)

val ( <= ) : lhs:t -> rhs:t -> bool
(** The comparison operator of abstract states. *)

val join : t -> t -> t
(** The join operator of abstract states. *)

val widen : prev:t -> next:t -> num_iters:int -> t
(** The widen operator of abstract states. *)
