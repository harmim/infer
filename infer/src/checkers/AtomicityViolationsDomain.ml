(** Detection of atomicity violations domain implementation. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd
open! AtomicityUtils

module F = Format
module IC = In_channel
module L = List
module Loc = Location
module P = Pervasives
module S = String
module Set = Caml.Set

(* ****************************** Types ************************************* *)

(** A pair of atomic functions. *)
type atomicPair = string * string

(** A pair of atomic functions with its location. *)
type atomicPairLoc = {pair: atomicPair; line: int; col: int; file: string}

(** A pair of atomic functions with an access path. *)
type atomicPairWithPath = atomicPair * AccessPath.t

(* ****************************** Functions ********************************* *)

(** Makes a pair of atomic funcions with its location based on Location. *)
let make_atomic_pair_loc (p : atomicPair) (loc : Loc.t) : atomicPairLoc =
  {pair= p; line= loc.line; col= loc.col; file= SourceFile.to_abs_path loc.file}

(** Pushes an element into an atomic pair. *)
let atomic_pair_push (p : atomicPair) (s : string) : atomicPair = P.snd p, s

(** Checks whether atomic pairs are equal. *)
let atomic_pairs_eq
  ((p1Fst : string), (p1Snd : string) : atomicPair)
  ((p2Fst : string), (p2Snd : string) : atomicPair)
  : bool = S.equal p1Fst p2Fst && S.equal p1Snd p2Snd

(** An empty pair of atomic functions. *)
let emptyAtomicPair = "", ""

(* ****************************** Modules *********************************** *)

(** A set of pairs of atomic functions. *)
module AtomicPairSet = Set.Make (struct
  type t = atomicPair

  let compare (p1 : t) (p2 : t) : int =
    if atomic_pairs_eq p1 p2 then 0 else if P.compare p1 p2 > 0 then 1 else -1
end)

(** A set of pairs of atomic functions with its location. *)
module AtomicPairLocSet = Set.Make (struct
  type t = atomicPairLoc

  let compare (p1 : t) (p2 : t) : int =
    if
      atomic_pairs_eq p1.pair p2.pair
      && phys_equal p1.line p2.line
      && phys_equal p1.col p2.col
      && S.equal p1.file p2.file
    then 0
    else if P.compare p1 p2 > 0 then 1
    else -1
end)

(** A set of pairs of atomic functions with an access path. *)
module AtomicPairWithPathSet = Set.Make (struct
  type t = atomicPairWithPath

  let compare
    ((p1 : atomicPair), (path1 : AccessPath.t) : t)
    ((p2 : atomicPair), (path2 : AccessPath.t) : t)
    : int =
    if atomic_pairs_eq p1 p2 && AccessPath.equal path1 path2 then 0
    else if P.compare p1 p2 > 0 then 1
    else -1
end)

(* ****************************** Global data ******************************* *)

(** A type of global data. *)
type globalData = {initialised: bool; atomicPairs: AtomicPairSet.t}

(** A global data reference. *)
let globalData : globalData ref =
  ref {initialised= false; atomicPairs= AtomicPairSet.empty}

(** Checks whether an atomic pair is violating atomicity. *)
let check_violating_atomicity
  ?(checkFirstEmpty : bool = false)
  (p : atomicPair)
  (atomicityViolations : AtomicPairLocSet.t ref)
  (lockedLastPairs : AtomicPairWithPathSet.t)
  (loc : Loc.t)
  : unit =
  let check (p : atomicPair) : unit =
    let is_pair_locked : bool =
      let locked : bool ref = ref false in

      let iterator
        ((p' : atomicPair), (_ : AccessPath.t) : atomicPairWithPath) : unit =
        if atomic_pairs_eq p' p then locked := true
      in
      AtomicPairWithPathSet.iter iterator lockedLastPairs;

      !locked
    in

    if AtomicPairSet.mem p !globalData.atomicPairs && not is_pair_locked then
      atomicityViolations :=
        AtomicPairLocSet.add (make_atomic_pair_loc p loc) !atomicityViolations
  in

  check p;
  if checkFirstEmpty then check ("", P.snd p)

(* ****************************** Initialisation **************************** *)

let initialise (_ : bool) : unit =
  if not !globalData.initialised then
  (
    (* Check existence of the input file with atomic sets. *)
    ( match Sys.file_exists atomicSetsFile with
      | `Yes -> ()

      | _ ->
        Logging.(die UserError)
          "File '%s' does not exist. Run the detection of atomic sets first \
           using '--atomic-sets-only'."
          atomicSetsFile
    );

    let atomicPairs : AtomicPairSet.t ref = ref AtomicPairSet.empty in

    (* Read atomic pairs from the input file. *)
    let ic : IC.t = IC.create ~binary:false atomicSetsFile
    and read_line (l : string) : unit =
      (* Truncate the function name and split by atomic sets. *)
      let sets : string list =
        Str.split
          (Str.regexp "} {") (Str.replace_first (Str.regexp "^.+: ") "" l)
      in

      let iterator (set : string) : unit =
        (* Truncate parentheses and commas and split by functions. *)
        let functions : string list =
          Str.split
            (Str.regexp " ") (Str.global_replace (Str.regexp "}\|{\|,") "" set)
        in
        let functionsCount : int = L.length functions in

        if phys_equal functionsCount 1 then
          atomicPairs :=
            AtomicPairSet.add ("", (L.nth_exn functions 0)) !atomicPairs
        else
          for i = 0 to functionsCount - 1 do
            for j = i + 1 to functionsCount - 1 do
              atomicPairs :=
                AtomicPairSet.add
                  (L.nth_exn functions i, L.nth_exn functions j)
                  !atomicPairs;

              atomicPairs :=
                AtomicPairSet.add
                  (L.nth_exn functions j, L.nth_exn functions i)
                  !atomicPairs
            done
          done
      in
      L.iter sets ~f:iterator
    in
    IC.iter_lines ~fix_win_eol:true ic ~f:read_line;
    IC.close ic;

    globalData := {initialised= true; atomicPairs= !atomicPairs}
  )

(* ****************************** Astate ************************************ *)

(** An element of an abstract state. *)
type tElement =
  { firstCall: string
  ; lastPair: atomicPair
  ; nestedLastCalls: SSet.t
  ; atomicityViolations: AtomicPairLocSet.t
  ; lockedLastPairs: AtomicPairWithPathSet.t }

(** A set of types tElement is an abstract state. *)
module TSet = Set.Make (struct
  type t = tElement

  let compare (e1 : t) (e2 : t) : int =
    if
      S.equal e1.firstCall e2.firstCall
      && atomic_pairs_eq e1.lastPair e2.lastPair
      && SSet.equal e1.nestedLastCalls e2.nestedLastCalls
      && AtomicPairLocSet.equal e1.atomicityViolations e2.atomicityViolations
      && AtomicPairWithPathSet.equal e1.lockedLastPairs e2.lockedLastPairs
    then 0
    else if P.compare e1 e2 > 0 then 1
    else -1
end)

type t = TSet.t

let initial : t =
  (* An initial abstract state is a set with a single empty element. *)
  TSet.singleton
    { firstCall= ""
    ; lastPair= emptyAtomicPair
    ; nestedLastCalls= SSet.empty
    ; atomicityViolations= AtomicPairLocSet.empty
    ; lockedLastPairs= AtomicPairWithPathSet.empty }

let pp (fmt : F.formatter) (astate : t) : unit =
  let iterator (astateEl : tElement) : unit =
    F.pp_print_string fmt "{\n";

    (* firstCall *)
    F.fprintf fmt "%s;\n" astateEl.firstCall;

    (* lastPair *)
    F.fprintf
      fmt "(%s, %s);\n" (P.fst astateEl.lastPair) (P.snd astateEl.lastPair);

    (* nestedLastCalls *)
    F.fprintf
      fmt
      "{%s};\n"
      (S.concat (SSet.elements astateEl.nestedLastCalls) ~sep:", ");

    (* atomicityViolations *)
    let lastAtomicityViolationsPairOption : atomicPairLoc option =
      AtomicPairLocSet.max_elt_opt astateEl.atomicityViolations
    in
    let print_atomicity_violations_pair (p : atomicPairLoc) : unit =
      F.fprintf
        fmt
        "%s:%i:%i -> (%s, %s)"
        p.file p.line p.col (P.fst p.pair) (P.snd p.pair);

      match lastAtomicityViolationsPairOption with
      | Some (lastAtomicityViolationsPair : atomicPairLoc) ->
        if not (phys_equal p lastAtomicityViolationsPair) then
          F.pp_print_string fmt " | "

      | None -> ()
    in
    AtomicPairLocSet.iter
      print_atomicity_violations_pair astateEl.atomicityViolations;
    F.pp_print_string fmt ";\n";

    (* lockedLastPairs *)
    let lastLockedLastPairOption : atomicPairWithPath option =
      AtomicPairWithPathSet.max_elt_opt astateEl.lockedLastPairs
    in
    let print_locked_last_pair
      ( ((pFst : string), (pSnd : string) : atomicPair)
      , (path : AccessPath.t) : atomicPairWithPath )
      : unit = F.fprintf fmt "%a: (%s, %s)" AccessPath.pp path pFst pSnd;

      match lastLockedLastPairOption with
      | Some (lastLockedLastPair : atomicPairWithPath) ->
        if not (phys_equal ((pFst, pSnd), path) lastLockedLastPair) then
          F.pp_print_string fmt " | "

      | None -> ()
    in
    AtomicPairWithPathSet.iter print_locked_last_pair astateEl.lockedLastPairs;
    F.pp_print_string fmt ";\n";

    F.pp_print_string fmt "}\n"
  in
  TSet.iter iterator astate;

  F.pp_print_string fmt "\n\n"

let update_astate_on_function_call (astate : t) (f : string) (loc : Loc.t) : t =
  let mapper (astateEl : tElement) : tElement =
    let firstCall : string =
      if S.is_empty astateEl.firstCall then f else astateEl.firstCall
    and lastPair : atomicPair = atomic_pair_push astateEl.lastPair f
    and atomicityViolations : AtomicPairLocSet.t ref =
      ref astateEl.atomicityViolations
    and lockedLastPairs : AtomicPairWithPathSet.t =
      (* Updates pairs of atomic functions. *)
      let mapper
        ((p : atomicPair), (path : AccessPath.t) : atomicPairWithPath)
        : atomicPairWithPath = atomic_pair_push p f, path
      in
      AtomicPairWithPathSet.map mapper astateEl.lockedLastPairs
    in

    (* Check whether the last pair is violating atomicity. *)
    check_violating_atomicity
      lastPair atomicityViolations lockedLastPairs loc ~checkFirstEmpty:true;

    let iterator (lastCall : string) : unit =
      let p : atomicPair = lastCall, f in
      let lockedLastPairs : AtomicPairWithPathSet.t =
        let mapper
          ((p' : atomicPair), (path : AccessPath.t) : atomicPairWithPath)
          : atomicPairWithPath = (if S.is_empty (P.fst p') then p' else p), path
        in
        AtomicPairWithPathSet.map mapper lockedLastPairs
      in

      (* Check whether each pair begining with the nested last call and
          ending with the current function call is violating atomicity. *)
      check_violating_atomicity p atomicityViolations lockedLastPairs loc
    in
    SSet.iter iterator astateEl.nestedLastCalls;

    (* Update the first call, the last pair, the atomicity violations, the
       locked last pairs, and clear the nested last calls. *)
    { firstCall= firstCall
    ; lastPair= lastPair
    ; nestedLastCalls= SSet.empty
    ; atomicityViolations= !atomicityViolations
    ; lockedLastPairs= lockedLastPairs }
  in
  TSet.map mapper astate

let update_astate_on_lock (astate : t) (lockPath : AccessPath.t) : t =
  let mapper (astateEl : tElement) : tElement =
    let lockedLastPairs : AtomicPairWithPathSet.t =
      AtomicPairWithPathSet.add
        (emptyAtomicPair, lockPath) astateEl.lockedLastPairs
    in

    {astateEl with lockedLastPairs= lockedLastPairs}
  in
  TSet.map mapper astate

let update_astate_on_unlock (astate : t) (lockPath : AccessPath.t) : t =
  let mapper (astateEl : tElement) : tElement =
    let lockedLastPairs : AtomicPairWithPathSet.t =
      let filter
        ((_ : atomicPair), (path : AccessPath.t) : atomicPairWithPath) : bool =
        not (AccessPath.equal path lockPath)
      in
      AtomicPairWithPathSet.filter filter astateEl.lockedLastPairs
    in

    {astateEl with lockedLastPairs= lockedLastPairs}
  in
  TSet.map mapper astate

(* ****************************** Summary *********************************** *)

type summary = {firstCalls: SSet.t; lastCalls: SSet.t}

let pp_summary (fmt : F.formatter) (summary : summary) : unit =
  (* firstCalls *)
  F.fprintf
    fmt
    "firstCalls: {%s}\n"
    (S.concat (SSet.elements summary.firstCalls) ~sep:", ");

  (* lastCalls *)
  F.fprintf
    fmt
    "lastCalls: {%s}\n\n\n"
    (S.concat (SSet.elements summary.lastCalls) ~sep:", ")

let update_astate_on_function_call_with_summary
  (astate : t) (summary : summary) (loc : Loc.t) : t =
  (* Add the last calls from a given summary to the nested last calls of the
     abstract state and check for atomicity violations with the first calls of
     a given summary. *)
  if SSet.is_empty summary.firstCalls && SSet.is_empty summary.lastCalls then
    astate
  else
    let mapper (astateEl : tElement) : tElement =
      let atomicityViolations : AtomicPairLocSet.t ref =
        ref astateEl.atomicityViolations
      and lastCall : string = P.snd astateEl.lastPair in

      let iterator (firstCall : string) : unit =
        let p : atomicPair = lastCall, firstCall in
        let lockedLastPairs : AtomicPairWithPathSet.t =
          let mapper
            ((_ : atomicPair), (path : AccessPath.t) : atomicPairWithPath)
            : atomicPairWithPath = p, path
          in
          AtomicPairWithPathSet.map mapper astateEl.lockedLastPairs
        in

        (* Check whether each pair begining with the last called function
           and ending witch the first call of a given summary is violating
           atomicity. *)
        check_violating_atomicity p atomicityViolations lockedLastPairs loc
      in
      SSet.iter iterator summary.firstCalls;

      { astateEl with
        nestedLastCalls= summary.lastCalls
      ; atomicityViolations= !atomicityViolations }
    in
    TSet.map mapper astate

let convert_astate_to_summary (astate : t) : summary =
  (* Derivates the first calls and the last calls from the first calls and from
     the last pairs of elements of the abstract state. *)
  let firstCalls : SSet.t ref = ref SSet.empty
  and lastCalls : SSet.t ref = ref SSet.empty in

  let iterator (astateEl : tElement) : unit =
    if not (S.is_empty astateEl.firstCall) then
      firstCalls := SSet.add astateEl.firstCall !firstCalls;

    if not (S.is_empty (P.snd astateEl.lastPair)) then
      lastCalls := SSet.add (P.snd astateEl.lastPair) !lastCalls
  in
  TSet.iter iterator astate;

  {firstCalls= !firstCalls; lastCalls= !lastCalls}

let report_atomicity_violations
  (astate : t) (report : (Loc.t -> string -> unit)) : unit =
  (* Report atomicity violations from atomicity violations stored in the
     abstract state. *)
  let iterator (astateEl : tElement) : unit =
    let iterator (p : atomicPairLoc) : unit =
      let fst : string = P.fst p.pair and snd : string = P.snd p.pair in

      if not (S.is_empty fst) || not (S.is_empty snd) then
        let loc : Loc.t =
          {line= p.line; col= p.col; file= SourceFile.from_abs_path p.file}
        and msg : string =
          if not (S.is_empty fst) && not (S.is_empty snd) then
            F.asprintf
              "Atomicity Violation! - Functions '%s' and '%s' should be called \
               atomically."
              fst snd
          else
            F.asprintf
              "Atomicity Violation! - Function '%s' should be called \
               atomically."
              (if S.is_empty fst then snd else fst)
        in

        report loc msg
    in
    AtomicPairLocSet.iter iterator astateEl.atomicityViolations
  in
  TSet.iter iterator astate

(* ****************************** Operators ********************************* *)

(* The lhs is less or equal to the rhs if the lhs is a subset of the rhs. *)
let ( <= ) ~lhs:(l : t) ~rhs:(r : t) : bool = TSet.subset l r

let join (astate1 : t) (astate2 : t) : t =
  (* Union of abstract states. *)
  (* let result : t = *)
  TSet.union astate1 astate2
  (* in *)

  (* F.fprintf
    F.std_formatter
    "\n\nJoin:\n1:\n%a\n2:\n%a\nresult:\n%a\n\n"
    pp astate1 pp astate2 pp result; *)

  (* result *)

let widen ~prev:(p : t) ~next:(n : t) ~num_iters:(i : int) : t =
  (* Join previous and next abstract states. *)
  if P.( <= ) i Config.atomicity_violations_widen_limit then join p n else p
