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

(** A type of a structure that holds function names loaded from a file. *)
type functionsFromFile = {initialised: bool; names: SSet.t}

(** A reference to a structure that holds functions that should be ignored. *)
let ignoredFunctions : functionsFromFile ref =
  ref {initialised= false; names= SSet.empty}

(** A reference to a structure that holds functions that should be allowed. *)
let allowedFunctions : functionsFromFile ref =
  ref {initialised= false; names= SSet.empty}

(** The initialisation of a structure that holds function names loaded from
    a file. *)
let initialiseFunctionsFromFile
  (functions : functionsFromFile ref) (fileOpt : string option) : unit =
  if not !functions.initialised then (
    let names : SSet.t ref = ref SSet.empty in

    ( match fileOpt with
      Some (file : string) ->
        ( match Sys.file_exists file with
          `Yes -> ()

          | _ ->
            Logging.(die UserError)
              "File '%s' that should contain function names does not exist."
              file
        );

        let ic : In_channel.t = In_channel.create ~binary:false file in
        In_channel.iter_lines ~fix_win_eol:true ic ~f:(
          fun (f : string) : unit -> names := SSet.add f !names );
        In_channel.close ic

      | None -> ()
    );

    functions := {initialised= true; names= !names}
  )

let f_is_ignored (f : Procname.t) : bool =
  initialiseFunctionsFromFile
    ignoredFunctions Config.atomicity_ignored_functions_file;
  initialiseFunctionsFromFile
    allowedFunctions Config.atomicity_allowed_functions_file;

  let fString : string = Procname.to_string f in

  if str_contains fString Config.clang_inner_destructor_prefix then true
  else if str_contains fString Config.clang_initializer_prefix then true
  else if Procname.equal f BuiltinDecl.__set_locked_attribute then false
  else if Procname.equal f BuiltinDecl.__delete_locked_attribute then false
  else if str_contains fString "__" && BuiltinDecl.is_declared f then true
  else if SSet.mem fString !ignoredFunctions.names then true
  else if
    not (SSet.is_empty !allowedFunctions.names)
    && not (SSet.mem fString !allowedFunctions.names)
  then true
  else if Procname.is_constructor f then true
  else false

let get_lock_path (exp : HilExp.t) : AccessPath.t option =
  match HilExp.get_access_exprs exp with
  (accessExp :: _ : AccessExp.t list) ->
    Some (AccessExp.to_access_path accessExp)

  | _ -> None
