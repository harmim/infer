(** Atomicity violations analysis utilities implementation. *)

open! IStd

module S = String

let inferDir : string =
  Escape.escape_filename (CommandLineOption.init_work_dir ^ "/infer-out")

let strings_equal (s1 : string) (s2 : string) : bool =
  phys_equal (S.compare s1 s2) 0

let is_lock (f : string) : bool = strings_equal f "pthread_mutex_lock"

let is_unlock (f : string) : bool = strings_equal f "pthread_mutex_unlock"
