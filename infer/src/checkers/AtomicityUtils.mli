(** Atomicity violations analysis utilities interface. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd

module S = String
module Set = Caml.Set

(* ****************************** Modules *********************************** *)

module SSet : module type of Set.Make (S)
(** A set of strings. *)

(* ****************************** Constants ********************************* *)

val inferDir : string
(** The Infer work directory. *)

val atomicSetsFile : string
(** A file for storing atomic sets. *)

(* ****************************** Functions ********************************* *)

val str_contains : string -> string -> bool
(** Checks whether the second string is a substring of the first string. *)

val f_is_ignored : Procname.t -> bool
(** Checks whether a given function is ignored. *)

val get_lock_path : HilExp.t -> AccessPath.t option
(** Returns an access path of a given expression. *)
