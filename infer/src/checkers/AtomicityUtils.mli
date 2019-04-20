(** Atomicity violations analysis utilities interface. *)

open! IStd

val inferDir : string

val atomicSequencesFile : string

val s_eq : string -> string -> bool
(** Checks whether strings are equal. *)

val s_empty : string -> bool

val is_lock : string -> bool
(** Checks whether the given function is a lock. *)

val is_unlock : string -> bool
(** Checks whether the given function is an unlock. *)

val lists_eq : 'a list -> 'a list -> ('a -> 'a -> bool) -> bool
(** Checks whether lists are equal. *)

val string_lists_eq : string list -> string list -> bool
(** Checks whether string lists are equal. *)

val list_add_unique : 'a list -> 'a -> ('a -> 'a -> bool) -> 'a list
(** Adds the element to the list without duplicities. *)

val string_list_add_unique : string list -> string -> string list
(** Adds the string element to the list without duplicities. *)

val string_list_list_add_unique :
  (string list) list -> string list -> (string list) list
(** Adds the string list element to the list without duplicities. *)

val list_remove_last : 'a list -> 'a list
(** Removes a last element from the list. *)
