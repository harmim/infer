(** Atomicity violations analysis utilities interface. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd

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

(* ****************************** Lists ************************************* *)

val lists_eq : 'a list -> 'a list -> ('a -> 'a -> bool) -> bool
(** Checks whether lists are equal. *)

val string_lists_eq : string list -> string list -> bool
(** Checks whether string lists are equal. *)

val list_add_unique : 'a list -> 'a -> ('a -> 'a -> bool) -> 'a list
(** Adds an element to a list without duplicities. *)

val string_list_add_unique : string list -> string -> string list
(** Adds a string element to a list without duplicities. *)

val string_list_list_add_unique :
  (string list) list -> string list -> (string list) list
(** Adds a string list element to a list without duplicities. *)
