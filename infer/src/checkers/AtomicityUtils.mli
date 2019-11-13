(** Atomicity violations analysis utilities interface. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd

module Pname = Typ.Procname
module Set = Caml.Set

(* ****************************** Modules *********************************** *)

module SSet : module type of Set.Make (String)
(** Set of strings. *)

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

(* ****************************** Functions ********************************* *)

val f_is_lock : Pname.t -> bool
(** Checks whether a given function is a lock. *)

val f_is_unlock : Pname.t -> bool
(** Checks whether a given function is an unlock. *)

val f_is_ignored : Pname.t -> bool
(** Checkes whether a given function is ignored. *)
