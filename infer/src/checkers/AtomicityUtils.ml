(** Atomicity violations analysis utilities implementation. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd

module AccessExp = HilExp.AccessExpression
module S = String
module Set = Caml.Set

(* ****************************** Modules *********************************** *)

module SSet = Set.Make (S)

(* ****************************** Constants ********************************* *)

let inferDir : string = CommandLineOption.init_work_dir ^ "/infer-atomicity-out"

let atomicSetsFile : string = inferDir ^ "/atomic-sets"

(* ****************************** Functions ********************************* *)

let str_contains (s1 : string) (s2 : string) : bool =
  try ignore (Str.search_forward (Str.regexp_string s2) s1 0); true
  with Caml.Not_found -> false

let f_is_ignored ?(ignoreCall : bool = false) (f : Procname.t) : bool =
  let fString : string = Procname.to_string f in

  str_contains fString "__"
  && (
    str_contains fString Config.clang_inner_destructor_prefix
    || str_contains fString Config.clang_initializer_prefix
    || (BuiltinDecl.is_declared f && (not ignoreCall || not (
      Procname.equal f BuiltinDecl.__set_locked_attribute
      || Procname.equal f BuiltinDecl.__delete_locked_attribute
    )))
  )

let get_lock_path (exp : HilExp.t) : AccessPath.t option =
  match HilExp.get_access_exprs exp with
  (accessExp :: _ : AccessExp.t list) ->
    Some (AccessExp.to_access_path accessExp)

  | _ -> None
