(** Atomicity violations analysis utilities implementation. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd

module Pname = Typ.Procname
module S = String
module Set = Caml.Set

(* ****************************** Modules *********************************** *)

module SSet = Set.Make (String)

(* ****************************** Constants ********************************* *)

let inferDir : string =
  Escape.escape_filename
    (CommandLineOption.init_work_dir ^ "/infer-atomicity-out")

let atomicSetsFile : string = inferDir ^ "/atomic-sets"

(* ****************************** Strings *********************************** *)

let s_eq (s1 : string) (s2 : string) : bool = phys_equal (S.compare s1 s2) 0

let s_empty (s : string) : bool = s_eq s ""

(* ****************************** Functions ********************************* *)

let f_is_lock (f : Pname.t) : bool =
  let fString : string = Pname.to_string f in

  s_eq fString "pthread_mutex_lock"

let f_is_unlock (f : Pname.t) : bool =
  let fString : string = Pname.to_string f in

  s_eq fString "pthread_mutex_unlock"

let f_is_ignored (f : Pname.t) : bool =
  let fString : string = Pname.to_string f in

  S.is_prefix fString ~prefix:Config.clang_inner_destructor_prefix
  || S.is_prefix fString ~prefix:Config.clang_initializer_prefix
  || (S.is_prefix fString ~prefix:"__" && BuiltinDecl.is_declared f)
