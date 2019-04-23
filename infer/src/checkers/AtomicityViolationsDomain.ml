(** Detection of atomicity violations domain implementation. *)

open! IStd
open! AtomicityUtils

module F = Format
module IC = In_channel
module L = List
module Loc = Location
module P = Pervasives
module S = String

(* ****************************** Types ************************************* *)

(** Pair of atomic functions. *)
type atomicPair = (string * string)
[@@ deriving sexp]

(** Pair of atomic functions with its location. *)
type atomicPairLoc = {pair : atomicPair; line : int; col : int; file : string}
[@@ deriving sexp]

(** Makes a pair of atomic funcions with its location based on Location. *)
let make_atomic_pair_loc (p : atomicPair) (loc : Loc.t) : atomicPairLoc =
  {pair= p; line= loc.line; col= loc.col; file= SourceFile.to_abs_path loc.file}

(* ****************************** Functions ********************************* *)

(** Pushes an element into an atomic pair. *)
let atomic_pair_push (p : atomicPair) (s : string) : atomicPair = (P.snd p, s)

(** Checkes whether an atomic pair is not empty (both components are used). *)
let atomic_pair_not_empty (p : atomicPair) : bool =
  not (s_empty (P.fst p)) && not (s_empty (P.snd p))

(** Checkes whether atomic pairs are equal.  *)
let atomic_pairs_eq (p1 : atomicPair) (p2 : atomicPair) : bool =
  s_eq (P.fst p1) (P.fst p2) && s_eq (P.snd p1) (P.snd p2)

(* ****************************** Modules *********************************** *)

(** Set of a pair of atomic functions. *)
module AtomicPairSet = Set.Make (struct
  type t = atomicPair
  [@@ deriving sexp]

  let compare (s1 : t) (s2 : t) : int =
    if atomic_pairs_eq s1 s2 then 0 else if P.compare s1 s2 > 0 then 1 else -1
end)

(** Set of a pair of atomic functions with its location. *)
module AtomicPairLocSet = Set.Make (struct
  type t = atomicPairLoc
  [@@ deriving sexp]

  let compare (s1 : t) (s2 : t) : int =
    if
      atomic_pairs_eq s1.pair s2.pair
      && phys_equal s1.line s2.line
      && phys_equal s1.col s2.col
      && s_eq s1.file s2.file
    then 0
    else if P.compare s1 s2 > 0 then 1
    else -1
end)

(* ****************************** Global data ******************************* *)

(** Type of global data. *)
type globalData = {initialised : bool; atomicPairs : AtomicPairSet.t}

(** Global data reference. *)
let globalData : globalData ref =
  ref { initialised : bool= false
      ; atomicPairs : AtomicPairSet.t= AtomicPairSet.empty }

(* ****************************** Initialisation **************************** *)

let initialise (_ : bool) : unit =
  if not !globalData.initialised then
  (
    (* Check existence of the input file with atomic sequences. *)
    ( match Sys.file_exists atomicSequencesFile with
      | `Yes -> ()
      | _ ->
        Logging.(die UserError)
          "File '%s' does not exist. Run the detection of atomic sequences first using '--atomic-sequences-only'."
          atomicSequencesFile
    );

    let atomicPairs : AtomicPairSet.t ref = ref AtomicPairSet.empty in

    (* Read atomic pairs from the input file. *)
    let ic : IC.t = IC.create ~binary:false atomicSequencesFile
    and read_line (l : string) : unit =
      (* Truncate the function name and split by atomic sequences. *)
      let sequences : string list =
        Str.split
          (Str.regexp ") (") (Str.replace_first (Str.regexp "^.+: ") "" l)
      in

      let iterator (sequence : string) : unit =
        (* Truncate parentheses. *)
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

(** Element of an abstract state. *)
type tElement =
  { firstCall : string
  ; lastPair : atomicPair
  ; nastedLastCalls : string list
  ; atomicityViolations : AtomicPairLocSet.t
  ; isInLock : bool }
[@@ deriving sexp]

(** Set of type tElement is an abstract state. *)
module TSet = Set.Make (struct
  type t = tElement
  [@@ deriving sexp]

  let compare (s1 : t) (s2 : t) : int =
    if
      s_eq s1.firstCall s2.firstCall
      && atomic_pairs_eq s1.lastPair s2.lastPair
      && string_lists_eq s1.nastedLastCalls s2.nastedLastCalls
      && AtomicPairLocSet.equal s1.atomicityViolations s2.atomicityViolations
      && phys_equal s1.isInLock s2.isInLock
    then 0
    else if P.compare s1 s2 > 0 then 1
    else -1
end)

type t = TSet.t

let initial : t =
  (* Initial abstract state is a set with a single empty element. *)
  TSet.singleton
    { firstCall : string= ""
    ; lastPair : atomicPair= ("", "")
    ; nastedLastCalls : string list= []
    ; atomicityViolations : AtomicPairLocSet.t= AtomicPairLocSet.empty
    ; isInLock : bool= false }

let pp (fmt : F.formatter) (astate : t) : unit =
  let lastAstateEl : tElement = TSet.max_elt_exn astate in

  (* firstCall *)
  let print_first_call (astateEl : tElement) : unit =
    F.pp_print_string fmt astateEl.firstCall;
    if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "firstCall: {";
  TSet.iter astate ~f:print_first_call;
  F.pp_print_string fmt "}\n";

  (* lastPair *)
  let print_last_pair (astateEl : tElement) : unit =
    F.fprintf
      fmt "(%s, %s)" (P.fst astateEl.lastPair) (P.snd astateEl.lastPair);
    if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "lastPair: ";
  TSet.iter astate ~f:print_last_pair;
  F.pp_print_string fmt "\n";

  (* nastedLastCalls *)
  let print_nasted_last_calls (astateEl : tElement) : unit =
    F.fprintf fmt "{%s}" (S.concat astateEl.nastedLastCalls ~sep:" ");
    if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "nastedLastCalls: ";
  TSet.iter astate ~f:print_nasted_last_calls;
  F.pp_print_string fmt "\n";

  (* atomicityViolations *)
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

  (* isInLock *)
  let print_is_in_lock (astateEl : tElement) : unit =
    F.fprintf fmt "%B" astateEl.isInLock;
    if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt ", "
  in
  F.pp_print_string fmt "isInLock: [";
  TSet.iter astate ~f:print_is_in_lock;
  F.pp_print_string fmt "]\n\n"

let update_astate_on_function_call (astate : t) (f : string) (loc : Loc.t) : t =
  let mapper (astateEl : tElement) : tElement =
    (* Ignore a function call if an abstract state is in a lock. *)
    if astateEl.isInLock then astateEl
    else
      let firstCall : string =
        if s_empty astateEl.firstCall then f else astateEl.firstCall
      and lastPair : atomicPair = atomic_pair_push astateEl.lastPair f
      and atomicityViolations : AtomicPairLocSet.t ref =
        ref astateEl.atomicityViolations
      in

      (* Check whether the last pair is violating atomicity. *)
      if AtomicPairSet.mem !globalData.atomicPairs lastPair then
        atomicityViolations :=
          AtomicPairLocSet.add
            !atomicityViolations (make_atomic_pair_loc lastPair loc);

      let iterator (lastCall : string) : unit =
        let pair : atomicPair = (lastCall, f) in

        (* Check whether each pair begining with the nasted last
           call and ending with the current function call
           violating atomicity. *)
        if AtomicPairSet.mem !globalData.atomicPairs pair then
          atomicityViolations :=
            AtomicPairLocSet.add
              !atomicityViolations (make_atomic_pair_loc pair loc)
      in
      L.iter astateEl.nastedLastCalls ~f:iterator;

      (* Update the first call, the last pair, the atomicity violations,
         and clear the nasted last calls. *)
      { astateEl with
        firstCall= firstCall
      ; lastPair= lastPair
      ; nastedLastCalls= []
      ; atomicityViolations= !atomicityViolations }
  in
  TSet.map astate ~f:mapper

let update_astate_on_lock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    (* Ignore a lock call if an abstract state is already in a lock. *)
    if astateEl.isInLock then astateEl
    else
      (* Clear the last pair and the nasted last calls, and set 'isInLock'. *)
      {astateEl with lastPair= ("", ""); nastedLastCalls= []; isInLock= true}
  in
  TSet.map astate ~f:mapper

let update_astate_on_unlock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    (* Ignore an unlock call if an abstract state is not in a lock. *)
    if not astateEl.isInLock then astateEl
    else {astateEl with isInLock= false} (* Unset 'isInLock'. *)
  in
  TSet.map astate ~f:mapper

(* ****************************** Summary *********************************** *)

type summary = {firstCalls : string list; lastCalls : string list}

let pp_summary (fmt : F.formatter) (summary : summary) : unit =
  F.fprintf fmt "firstCalls: {%s}\n" (S.concat summary.firstCalls ~sep:" ");
  F.fprintf fmt "lastCalls: {%s}\n\n" (S.concat summary.lastCalls ~sep:" ")

let update_astate_on_function_call_with_summary
  (astate : t) (summary : summary) (loc : Loc.t) : t =
  (* Add the last calls from a given summary to the nasted last calls
     of the abstract state and check for atomicity violations with
     the first calls of a given summary. *)
  if L.is_empty summary.firstCalls && L.is_empty summary.lastCalls then astate
  else
    let mapper (astateEl : tElement) : tElement =
      let atomicityViolations : AtomicPairLocSet.t ref =
        ref astateEl.atomicityViolations
      and lastCall : string = P.snd astateEl.lastPair in

      let iterator (fistCall : string) : unit =
        let pair : atomicPair = (lastCall, fistCall) in

        (* Check whether each pair begining with the last called
           function and ending witch the first call of a given summary
           violating atomicity. *)
        if AtomicPairSet.mem !globalData.atomicPairs pair then
          atomicityViolations :=
            AtomicPairLocSet.add
              !atomicityViolations (make_atomic_pair_loc pair loc)
      in
      L.iter summary.firstCalls ~f:iterator;

      { astateEl with
        nastedLastCalls= summary.lastCalls
      ; atomicityViolations= !atomicityViolations }
    in
    TSet.map astate ~f:mapper

let convert_astate_to_summary (astate : t) : summary =
  (* Derivates the first calls and the last calls from the first calls and
     from the last pairs of elements of the abstract state. *)
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
  (* Report atomicity violations from atomicity violations stored
     in the abstract state. *)
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
  (* lhs is less or equal to rhs if lhs is subset of rhs. *)
  if phys_equal l r then true else TSet.is_subset l ~of_:r

let join (astate1 : t) (astate2 : t) : t =
  (* Union of abstract states. *)
  (* let result : t = *)
  if phys_equal astate1 astate2 then astate1
  else if TSet.is_empty astate1 then astate2
  else if TSet.is_empty astate2 then astate1
  else TSet.union astate1 astate2
  (* in *)

  (* F.fprintf
    F.std_formatter
    "\n\nJoin:\n1:\n%a\n2:\n%a\nresult:\n%a\n\n"
    pp astate1 pp astate2 pp result; *)

  (* result *)

let widen ~prev:(p : t) ~next:(n : t) ~num_iters:(_ : int) : t =
  (* Join previous and next abstract states. *)
  join p n
