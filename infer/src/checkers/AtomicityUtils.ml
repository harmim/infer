(** Atomicity violations analysis utilities implementation. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd

module S = String
module Set = Caml.Set

(* ****************************** Modules *********************************** *)

module SSet = Set.Make (String)

(* ****************************** Constants ********************************* *)

let inferDir : string =
  Escape.escape_filename
    (CommandLineOption.init_work_dir ^ "/infer-atomicity-out")

let atomicSetsFile : string =  inferDir ^ "/atomic-sets"

(* ****************************** Strings *********************************** *)

let s_eq (s1 : string) (s2 : string) : bool = phys_equal (S.compare s1 s2) 0

let s_empty (s : string) : bool = s_eq s ""

let is_lock (f : string) : bool = s_eq f "pthread_mutex_lock"

let is_unlock (f : string) : bool = s_eq f "pthread_mutex_unlock"
