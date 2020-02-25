(** Detection of atomic sets domain implementation. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd
open! AtomicityUtils

module F = Format
module OC = Out_channel
module P = Pervasives
module S = String
module Set = Caml.Set

(* ****************************** Types ************************************* *)

(** A pair of sets of calls. Calls without a lock followed by calls with
    a lock. *)
type callsPair = SSet.t * SSet.t

(** A pair of sets of calls with an access path. *)
type callsPairWithPath = callsPair * AccessPath.t

(* ****************************** Functions ********************************* *)

(** Checks whether pairs of sets of calls are equal. *)
let calls_pairs_eq
  ((p1Fst : SSet.t), (p1Snd : SSet.t) : callsPair)
  ((p2Fst : SSet.t), (p2Snd : SSet.t) : callsPair)
  : bool = SSet.equal p1Fst p2Fst && SSet.equal p1Snd p2Snd

(* ****************************** Modules *********************************** *)

(** A set of pairs of sets of calls. *)
module CallsPairSet = Set.Make (struct
  type t = callsPair

  let compare (p1 : t) (p2 : t) : int =
    if calls_pairs_eq p1 p2 then 0 else if P.compare p1 p2 > 0 then 1 else -1
end)

(** A set of sets of strings. *)
module SSSet = Set.Make (SSet)

(** A set of pairs of sets of calls with an access path. *)
module CallsPairWithPathSet = Set.Make (struct
  type t = callsPairWithPath

  let compare
    ((p1 : callsPair), (path1 : AccessPath.t) : t)
    ((p2 : callsPair), (path2 : AccessPath.t) : t)
    : int =
    if calls_pairs_eq p1 p2 && AccessPath.equal path1 path2 then 0
    else if P.compare p1 p2 > 0 then 1
    else -1
end)

(* ****************************** Astate ************************************ *)

(** An element of an abstract state. *)
type tElement =
  { calls: SSet.t
  ; callsPairs: CallsPairWithPathSet.t
  ; finalCallsPairs: CallsPairSet.t }

(** A set of types tElement is an abstract state. *)
module TSet = Set.Make (struct
  type t = tElement

  let compare (e1 : t) (e2 : t) : int =
    if
      SSet.equal e1.calls e2.calls
      && CallsPairWithPathSet.equal e1.callsPairs e2.callsPairs
      && CallsPairSet.equal e1.finalCallsPairs e2.finalCallsPairs
    then 0
    else if P.compare e1 e2 > 0 then 1
    else -1
end)

type t = TSet.t

let initial : t =
  (* An initial abstract state is a set with a single empty element. *)
  TSet.singleton
    { calls= SSet.empty
    ; callsPairs= CallsPairWithPathSet.empty
    ; finalCallsPairs= CallsPairSet.empty }

let pp (fmt : F.formatter) (astate : t) : unit =
  let iterator (astateEl : tElement) : unit =
    F.pp_print_string fmt "{\n";

    (* calls *)
    F.fprintf fmt "{%s};\n" (S.concat (SSet.elements astateEl.calls) ~sep:", ");

    (* callsPairs *)
    let lastCallsPairOption : callsPairWithPath option =
      CallsPairWithPathSet.max_elt_opt astateEl.callsPairs
    in
    let print_calls_pair
      ( ((pFst : SSet.t), (pSnd : SSet.t) : callsPair)
      , (path : AccessPath.t) : callsPairWithPath )
      : unit =
      let withoutLock : string = S.concat (SSet.elements pFst) ~sep:", "
      and withLock : string = S.concat (SSet.elements pSnd) ~sep:", " in
      F.fprintf fmt "%a: {%s} ( {%s} )" AccessPath.pp path withoutLock withLock;

      match lastCallsPairOption with
      | Some (lastCallsPair : callsPairWithPath) ->
        if not (phys_equal ((pFst, pSnd), path) lastCallsPair) then
          F.pp_print_string fmt " | "

      | None -> ()
    in
    CallsPairWithPathSet.iter print_calls_pair astateEl.callsPairs;
    F.pp_print_string fmt ";\n";

    (* finalCallsPairs *)
    let lastFinalCallsPairOption : callsPair option =
      CallsPairSet.max_elt_opt astateEl.finalCallsPairs
    in
    let print_final_calls_pair
      ((pFst : SSet.t), (pSnd : SSet.t) : callsPair) : unit =
      let withoutLock : string = S.concat (SSet.elements pFst) ~sep:", "
      and withLock : string = S.concat (SSet.elements pSnd) ~sep:", " in
      F.fprintf fmt "{%s} ( {%s} )" withoutLock withLock;

      match lastFinalCallsPairOption with
      | Some (lastFinalCallsPair : callsPair) ->
        if not (phys_equal (pFst, pSnd) lastFinalCallsPair) then
          F.pp_print_string fmt " | "

      | None -> ()
    in
    CallsPairSet.iter print_final_calls_pair astateEl.finalCallsPairs;
    F.pp_print_string fmt ";\n";

    F.pp_print_string fmt "}\n"
  in
  TSet.iter iterator astate;

  F.pp_print_string fmt "\n\n"

let update_astate_on_function_call (astate : t) (f : string) : t =
  let mapper (astateEl : tElement) : tElement =
      let calls : SSet.t = SSet.add f astateEl.calls
      and callsPairs : CallsPairWithPathSet.t =
        let mapper
          ( ((pFst : SSet.t), (pSnd : SSet.t) : callsPair)
          , (path : AccessPath.t) : callsPairWithPath )
          : callsPairWithPath =
          (pFst, SSet.add f pSnd), path
        in
        CallsPairWithPathSet.map mapper astateEl.callsPairs
      in

      (* Update the calls and the calls pairs. *)
      {astateEl with calls= calls; callsPairs= callsPairs}
  in
  TSet.map mapper astate

let update_astate_on_lock (astate : t) (lockPath : AccessPath.t) : t =
  let mapper (astateEl : tElement) : tElement =
    let callsPairs : CallsPairWithPathSet.t =
      CallsPairWithPathSet.add
        ((astateEl.calls, SSet.empty), lockPath) astateEl.callsPairs
    in

    (* Clear the calls and update the calls pairs. *)
    {astateEl with calls= SSet.empty; callsPairs= callsPairs}
  in
  TSet.map mapper astate

let update_astate_on_unlock (astate : t) (lockPath : AccessPath.t) : t =
  let mapper (astateEl : tElement) : tElement =
    let finalCallsPairs : CallsPairSet.t ref = ref astateEl.finalCallsPairs in
    let callsPairs : CallsPairWithPathSet.t =
      let filter
        ((p : callsPair), (path : AccessPath.t) : callsPairWithPath) : bool =
        if AccessPath.equal path lockPath then
        (
          finalCallsPairs := CallsPairSet.add p !finalCallsPairs;

          false
        )
        else true
      in
      CallsPairWithPathSet.filter filter astateEl.callsPairs
    in

    (* Clear the calls, update the calls pairs and the final calls pairs. *)
    { calls= SSet.empty
    ; callsPairs= callsPairs
    ; finalCallsPairs= !finalCallsPairs }
  in
  TSet.map mapper astate

let update_astate_at_the_end_of_function (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    let finalCallsPairs : CallsPairSet.t ref = ref astateEl.finalCallsPairs in

    let iterator
      ((p : callsPair), (_ : AccessPath.t) : callsPairWithPath) : unit =
      finalCallsPairs := CallsPairSet.add p !finalCallsPairs
    in
    CallsPairWithPathSet.iter iterator astateEl.callsPairs;

    if not (SSet.is_empty astateEl.calls) then
      finalCallsPairs :=
        CallsPairSet.add (astateEl.calls, SSet.empty) !finalCallsPairs;

    (* Clear the calls and the calls pairs, and update the final calls pairs. *)
    { calls= SSet.empty
    ; callsPairs= CallsPairWithPathSet.empty
    ; finalCallsPairs= !finalCallsPairs }
  in
  TSet.map mapper astate

(* ****************************** Summary *********************************** *)

type summary = {atomicFunctions: SSSet.t; allOccurrences: SSet.t}

let pp_summary (fmt : F.formatter) (summary : summary) : unit =
  (* atomicFunctions *)
  let lastAtomicFunctionsOption : SSet.t option =
    SSSet.max_elt_opt summary.atomicFunctions
  in
  let print_atomic_functions (atomicFunctions : SSet.t) : unit =
    F.fprintf fmt "{%s}" (S.concat (SSet.elements atomicFunctions) ~sep:", ");

    match lastAtomicFunctionsOption with
    | Some (lastAtomicFunctions : SSet.t) ->
      if not (phys_equal atomicFunctions lastAtomicFunctions) then
        F.pp_print_string fmt " "

    | None -> ()
  in
  F.pp_print_string fmt "atomicFunctions: ";
  SSSet.iter print_atomic_functions summary.atomicFunctions;
  F.pp_print_string fmt "\n";

  (* allOccurrences *)
  F.fprintf
    fmt
    "allOccurrences: {%s}\n\n\n"
    (S.concat (SSet.elements summary.allOccurrences) ~sep:", ")

let update_astate_on_function_call_with_summary
  (astate : t) (summary : summary) : t =
  (* Add all occurrences from a given summary to the calls pairs and to the
     calls of each element of the abstract state. *)
  if SSet.is_empty summary.allOccurrences then astate
  else
    let mapper (astateEl : tElement) : tElement =
      let calls : SSet.t = SSet.union astateEl.calls summary.allOccurrences
      and callsPairs : CallsPairWithPathSet.t =
        let mapper
          ( ((pFst : SSet.t), (pSnd : SSet.t) : callsPair)
          , (path : AccessPath.t) : callsPairWithPath )
          : callsPairWithPath =
          (pFst, SSet.union pSnd summary.allOccurrences), path
        in
        CallsPairWithPathSet.map mapper astateEl.callsPairs
      in

      (* Update the calls and the calls pairs. *)
      {astateEl with calls= calls; callsPairs= callsPairs}
    in
    TSet.map mapper astate

let convert_astate_to_summary (astate : t) : summary =
  (* Derivates atomic functions and all occurrences from the final calls pairs
     of elements of the abstract state. *)
  let atomicFunctions : SSSet.t ref = ref SSSet.empty
  and allOccurrences : SSet.t ref = ref SSet.empty in

  let iterator (astateEl : tElement) : unit =
    let iterator ((pFst : SSet.t), (pSnd : SSet.t) : callsPair) : unit =
      if not (SSet.is_empty pSnd) then
        atomicFunctions := SSSet.add pSnd !atomicFunctions;

      allOccurrences := SSet.union !allOccurrences (SSet.union pFst pSnd)
    in
    CallsPairSet.iter iterator astateEl.finalCallsPairs
  in
  TSet.iter iterator astate;

  {atomicFunctions= !atomicFunctions; allOccurrences= !allOccurrences}

let print_atomic_sets (oc : OC.t) (f : string) (summary : summary) : unit =
  OC.fprintf oc "%s: " f;

  let lastAtomicFunctionsOption : SSet.t option =
    SSSet.max_elt_opt summary.atomicFunctions
  in
  let print_atomic_functions (atomicFunctions : SSet.t) : unit =
    OC.fprintf oc "{%s}" (S.concat (SSet.elements atomicFunctions) ~sep:", ");

    match lastAtomicFunctionsOption with
    | Some (lastAtomicFunctions : SSet.t) ->
      if not (phys_equal atomicFunctions lastAtomicFunctions) then
        OC.output_string oc " "

    | None -> ()
  in
  SSSet.iter print_atomic_functions summary.atomicFunctions;

  OC.newline oc

(* ****************************** Operators ********************************* *)

(* The lhs is less or equal to the rhs if lhs is a subset of the rhs. *)
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
  if P.( <= ) i Config.atomic_sets_widen_limit then join p n else p
