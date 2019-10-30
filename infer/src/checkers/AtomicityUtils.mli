(** Atomicity violations analysis utilities interface. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd

module Set = Caml.Set

(* ****************************** Modules *********************************** *)

(** Set of strings. *)
module SSet : module type of Set.Make (String)

(* ****************************** Constants ********************************* *)

val inferDir : string
(** Infer work directory. *)

val atomicSetsFile : string
(** File for storing atomic sets. *)

(* ****************************** Strings *********************************** *)

val s_eq : string -> string -> bool
(** Checks whether strings are equal. *)

val s_empty : string -> bool
(** Checks whether a string is an empty string. *)

val is_lock : string -> bool
(** Checks whether a given function is a lock. *)

val is_unlock : string -> bool
(** Checks whether a given function is an unlock. *)
