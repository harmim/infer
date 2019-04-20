(** Detection of atomicity violations domain implementation. *)

open! IStd
open! AtomicityUtils

module F = Format
module P = Pervasives
module IC = In_channel
module L = List
module S = String
module Loc = Location

(* ****************************** Types ************************************* *)

type atomicPair = (string * string) [@@ deriving sexp]

(* ****************************** Functions ********************************* *)

let atomic_pair_push (p : atomicPair) (s : string) : atomicPair = (P.snd p, s)

let atomic_pair_not_empty (p : atomicPair) : bool =
  not (s_empty (P.fst p)) && not (s_empty (P.snd p))

(* ****************************** Global data ******************************* *)

module AtomicPairSet = Set.Make (struct
  type t = atomicPair [@@ deriving sexp]

  let compare (e1 : t) (e2 : t) : int =
    if s_eq (P.fst e1) (P.fst e2) && s_eq (P.snd e1) (P.snd e2) then 0
    else if P.compare e1 e2 > 0 then 1
    else -1
end)

type globalDataT = {initialised : bool; atomicPairs : AtomicPairSet.t}

let globalData : globalDataT ref =
  ref { initialised : bool= false
      ; atomicPairs : AtomicPairSet.t= AtomicPairSet.empty }

(* ****************************** Initialisation **************************** *)

let initialise (_ : bool) : unit =
  if not !globalData.initialised then
  (
    let atomicPairs : AtomicPairSet.t ref = ref AtomicPairSet.empty in

    ( match Sys.file_exists atomicSequencesFile with
      | `Yes -> ()
      | _ ->
        Logging.(die UserError)
          "File '%s' does not exist. Run the detection of atomic sequences first using '--atomic-sequences-only'."
          atomicSequencesFile
    );

    let ic : IC.t = IC.create ~binary:false atomicSequencesFile in
    let read_line (l : string) : unit =
      let sequences : string list =
        Str.split
          (Str.regexp ") (")
          (Str.replace_first (Str.regexp "^.+: ") "" l)
      in

      let iterator (sequence : string) : unit =
        let sequence : string =
          Str.global_replace (Str.regexp ")\|(") "" sequence
        in

        let pair : atomicPair ref = ref ("", "") in
        let iterator (f : string) : unit =
          pair := atomic_pair_push !pair f;

          if atomic_pair_not_empty !pair then
            atomicPairs := AtomicPairSet.add !atomicPairs !pair
        in
        L.iter (Str.split (Str.regexp " ") sequence) ~f:iterator
      in
      L.iter sequences ~f:iterator
    in
    IC.iter_lines ~fix_win_eol:true ic ~f:read_line;
    IC.close ic;

    globalData := {initialised= true; atomicPairs= !atomicPairs}
  )

(* ****************************** Astate ************************************ *)

type atomicPairLoc =
  { pair : atomicPair
  ; line : int
  ; col : int
  ; file : string } [@@ deriving sexp]

let atomic_pair_loc_make (pair : atomicPair) (loc : Loc.t) : atomicPairLoc =
  { pair= pair
  ; line= loc.line
  ; col= loc.col
  ; file= SourceFile.to_abs_path loc.file }

module AtomicPairLocSet = Set.Make (struct
  type t = atomicPairLoc [@@ deriving sexp]

  let compare (e1 : t) (e2 : t) : int =
    if
      s_eq (P.fst e1.pair) (P.fst e2.pair)
      && s_eq (P.snd e1.pair) (P.snd e2.pair)
      && phys_equal e1.line e2.line
      && phys_equal e1.col e2.col
      && s_eq e1.file e2.file
    then 0
    else if P.compare e1 e2 > 0 then 1
    else -1
end)

type tElement =
  { firstCall : string
  ; lastPair : atomicPair
  ; nastedLastCalls : string list
  ; atomicityViolations : AtomicPairLocSet.t
  ; isInLock : bool } [@@ deriving sexp]

module TSet = Set.Make (struct
  type t = tElement [@@ deriving sexp]

  let compare (e1 : t) (e2 : t) : int =
    if
      s_eq e1.firstCall e2.firstCall
      && s_eq (P.fst e1.lastPair) (P.fst e2.lastPair)
      && s_eq (P.snd e1.lastPair) (P.snd e2.lastPair)
      && string_lists_eq e1.nastedLastCalls e2.nastedLastCalls
      && AtomicPairLocSet.equal e1.atomicityViolations e2.atomicityViolations
      && phys_equal e1.isInLock e2.isInLock
    then 0
    else if P.compare e1 e2 > 0 then 1
    else -1
end)

type t = TSet.t

let initial : t =
  TSet.singleton
    { firstCall : string= ""
    ; lastPair : atomicPair= ("", "")
    ; nastedLastCalls : string list= []
    ; atomicityViolations : AtomicPairLocSet.t= AtomicPairLocSet.empty
    ; isInLock : bool= false }

let pp (fmt : F.formatter) (astate : t) : unit =
  let lastAstateEl : tElement = TSet.max_elt_exn astate in

  let print_first_call (astateEl : tElement) : unit =
    F.fprintf fmt "%s" astateEl.firstCall;
    if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "firstCall: ";
  TSet.iter astate ~f:print_first_call;
  F.pp_print_string fmt "\n";

  let print_last_pair (astateEl : tElement) : unit =
    F.fprintf
      fmt "(%s, %s)" (P.fst astateEl.lastPair) (P.snd astateEl.lastPair);
    if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "lastPair: ";
  TSet.iter astate ~f:print_last_pair;
  F.pp_print_string fmt "\n";

  let print_nasted_last_calls (astateEl : tElement) : unit =
    F.fprintf fmt "{%s}" (S.concat astateEl.nastedLastCalls ~sep:" ");
    if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "nastedLastCalls: ";
  TSet.iter astate ~f:print_nasted_last_calls;
  F.pp_print_string fmt "\n";

  let print_atomicity_violations (astateEl : tElement) : unit =
    F.pp_print_string fmt "{";

    let lastAtomicityViolationPairOption : atomicPairLoc option =
      AtomicPairLocSet.max_elt astateEl.atomicityViolations
    in
    let print_atomicity_violation_pair (p : atomicPairLoc) : unit =
      F.fprintf
        fmt
        "%s:%i:%i (%s, %s)"
        p.file p.line p.col (P.fst p.pair) (P.snd p.pair);

      match lastAtomicityViolationPairOption with
      | Some (lastAtomicityViolationPair : atomicPairLoc) ->
        if not (phys_equal p lastAtomicityViolationPair) then
          F.pp_print_string fmt " | "

      | None -> ()
    in
    AtomicPairLocSet.iter
      astateEl.atomicityViolations ~f:print_atomicity_violation_pair;

    F.pp_print_string fmt "}";
    if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "atomicityViolations: ";
  TSet.iter astate ~f:print_atomicity_violations;
  F.pp_print_string fmt "\n";

  let print_is_in_lock (astateEl : tElement) : unit =
    F.fprintf fmt "%B" astateEl.isInLock;
    if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "isInLock: ";
  TSet.iter astate ~f:print_is_in_lock;
  F.pp_print_string fmt "\n\n"

let update_astate_on_function_call (astate : t) (f : string) (loc : Loc.t) : t =
  let mapper (astateEl : tElement) : tElement =
      if astateEl.isInLock then astateEl
      else
        let firstCall : string =
          if s_empty astateEl.firstCall then f
          else astateEl.firstCall
        and lastPair : atomicPair = atomic_pair_push astateEl.lastPair f
        and atomicityViolations : AtomicPairLocSet.t ref =
          ref astateEl.atomicityViolations
        in

        if AtomicPairSet.mem !globalData.atomicPairs lastPair then
          atomicityViolations :=
            AtomicPairLocSet.add
              !atomicityViolations (atomic_pair_loc_make lastPair loc);

        let iterator (lastCall : string) : unit =
          let pair : atomicPair = (lastCall, f) in

          if AtomicPairSet.mem !globalData.atomicPairs pair then
            atomicityViolations :=
              AtomicPairLocSet.add
                !atomicityViolations (atomic_pair_loc_make pair loc)
        in
        L.iter astateEl.nastedLastCalls ~f:iterator;

        { astateEl with
          firstCall= firstCall
        ; lastPair= lastPair
        ; nastedLastCalls= []
        ; atomicityViolations= !atomicityViolations }
  in
  TSet.map astate ~f:mapper

let update_astate_on_lock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    if astateEl.isInLock then astateEl
    else
      {astateEl with lastPair= ("", "") ; nastedLastCalls= []; isInLock= true}
  in
  TSet.map astate ~f:mapper

let update_astate_on_unlock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    if not astateEl.isInLock then astateEl
    else {astateEl with isInLock= false}
  in
  TSet.map astate ~f:mapper

(* ****************************** Summary *********************************** *)

type summary = {firstCalls : string list; lastCalls : string list}

let pp_summary (fmt : F.formatter) (summary : summary) : unit =
  F.fprintf fmt "firstCalls: %s\n" (S.concat summary.firstCalls ~sep:" ");
  F.fprintf fmt "lastCalls: %s\n\n" (S.concat summary.lastCalls ~sep:" ")

let update_astate_on_function_call_with_summary
  (astate : t) (summary : summary) (loc : Loc.t) : t =
  if L.is_empty summary.firstCalls && L.is_empty summary.lastCalls then astate
  else
    let mapper (astateEl : tElement) : tElement =
      let atomicityViolations : AtomicPairLocSet.t ref =
        ref astateEl.atomicityViolations
      and lastCall : string = P.snd astateEl.lastPair in

      let iterator (fistCall : string) : unit =
        let pair : atomicPair = (lastCall, fistCall) in

        if AtomicPairSet.mem !globalData.atomicPairs pair then
          atomicityViolations :=
            AtomicPairLocSet.add
              !atomicityViolations (atomic_pair_loc_make pair loc)
      in
      L.iter summary.firstCalls ~f:iterator;

      { astateEl with
        nastedLastCalls= summary.lastCalls
      ; atomicityViolations= !atomicityViolations }
    in
    TSet.map astate ~f:mapper

let convert_astate_to_summary (astate : t) : summary =
  let firstCalls : (string list) ref = ref []
  and lastCalls : (string list) ref = ref [] in

  let iterator (astateEl : tElement) : unit =
    if not (s_empty astateEl.firstCall) then
      firstCalls := string_list_add_unique !firstCalls astateEl.firstCall;

    if not (s_empty (P.snd astateEl.lastPair)) then
      lastCalls := string_list_add_unique !lastCalls (P.snd astateEl.lastPair)
  in
  TSet.iter astate ~f:iterator;

  {firstCalls= !firstCalls; lastCalls= !lastCalls}

let report_atomicity_violations
  (astate : t) (report : (Loc.t -> string -> unit)) : unit =
  let iterator (astateEl : tElement) : unit =
    let iterator (p : atomicPairLoc) : unit =
      let loc : Loc.t =
        {line= p.line; col= p.col; file= SourceFile.from_abs_path p.file}
      and msg : string =
        F.asprintf
          "Atomicity Violation! - Functions '%s' and '%s' should be called atomically."
          (P.fst p.pair) (P.snd p.pair)
      in

      report loc msg
    in
    AtomicPairLocSet.iter astateEl.atomicityViolations ~f:iterator
  in
  TSet.iter astate ~f:iterator

(* ****************************** Operators ********************************* *)

let ( <= ) ~lhs:(l : t) ~rhs:(r : t) : bool =
  if phys_equal l r then true else TSet.is_subset l ~of_:r

let join (astate1 : t) (astate2 : t) : t =
  (* let result : t = *)
  if phys_equal astate1 astate2 then astate1
  else if TSet.is_empty astate1 then astate2
  else if TSet.is_empty astate2 then astate1
  else TSet.union astate1 astate2
  (* in *)

  (* F.fprintf F.std_formatter "\nJoin:\n";
  F.fprintf F.std_formatter "\n1:\n%a" pp astate1;
  F.fprintf F.std_formatter "\n2:\n%a" pp astate2;
  F.fprintf F.std_formatter "\nresult:\n%a\n\n" pp result; *)

  (* result *)

let widen ~prev:(p : t) ~next:(n : t) ~num_iters:(_ : int) : t = join p n
