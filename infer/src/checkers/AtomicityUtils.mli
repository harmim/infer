(** Atomicity violations analysis utilities interface. *)

open! IStd

val inferDir : string

val strings_equal : string -> string -> bool
(** Checks whether strings are equal. *)

val is_lock : string -> bool
(** Checks whether the given function is a lock. *)

val is_unlock : string -> bool
(** Checks whether the given function is an unlock. *)
