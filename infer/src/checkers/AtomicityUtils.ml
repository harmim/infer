(** Atomicity violations analysis utilities implementation. *)

open! IStd

module S = String
module L = List

let inferDir : string =
  Escape.escape_filename
    (CommandLineOption.init_work_dir ^ "/infer-atomicity-out")

let atomicSequencesFile : string =  inferDir ^ "/atomic-sequences"

let s_eq (s1 : string) (s2 : string) : bool = phys_equal (S.compare s1 s2) 0

let s_empty (s : string) = s_eq s ""

let is_lock (f : string) : bool = s_eq f "pthread_mutex_lock"

let is_unlock (f : string) : bool = s_eq f "pthread_mutex_unlock"

let lists_eq (l1 : 'a list) (l2 : 'a list) (cmp : ('a -> 'a -> bool)) : bool =
  (* A length of lists and theirs elements must be equal. *)
  if not (phys_equal (L.length l1) (L.length l2)) then false
  else
  (
    let eq : bool ref = ref true in

    L.iter2_exn
      l1 l2 ~f:( fun (e1 : 'a) (e2 : 'a) : unit ->
        if not (cmp e1 e2) then eq := false );

    !eq
  )

let string_lists_eq (l1 : string list) (l2 : string list) : bool =
  lists_eq l1 l2 s_eq

let list_add_unique (l : 'a list) (e : 'a) (eq : ('a -> 'a -> bool)) : 'a list =
  if L.mem l e ~equal:eq then l else l @ [e]

let string_list_add_unique (l : string list) (f : string) : string list =
  list_add_unique l f s_eq

let string_list_list_add_unique
  (ll : (string list) list) (l : string list) : (string list) list =
  list_add_unique ll l string_lists_eq

let list_remove_last (l : 'a list) : 'a list =
  let lLength : int = L.length l in

  L.filteri
    l ~f:(fun (i : int) (_ : 'a) : bool -> not (phys_equal i (lLength - 1)))
