(** Atomicity violations analysis utilities interface. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd

module Pname = Typ.Procname
module Set = Caml.Set

(* ****************************** Modules *********************************** *)

module SSet : module type of Set.Make (String)
(** A set of strings. *)

(* ****************************** Constants ********************************* *)

val inferDir : string
(** The Infer work directory. *)

val atomicSetsFile : string
(** A file for storing atomic sets. *)

(* ****************************** Functions ********************************* *)

val f_is_lock : Pname.t -> bool
(** Checks whether a given function is a lock. *)

val f_is_unlock : Pname.t -> bool
(** Checks whether a given function is an unlock. *)

val f_is_ignored : Pname.t -> bool
(** Checks whether a given function is ignored. *)
