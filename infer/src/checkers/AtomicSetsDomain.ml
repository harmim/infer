(** Detection of atomic sets domain implementation. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd
open! AtomicityUtils

module F = Format
module Opt = Option
module S = String
module Set = Caml.Set

(* ****************************** Types ************************************* *)

(** A pair of sets of calls. Calls without a lock followed by calls with
    a lock. *)
type callsPair = SSet.t * SSet.t

(** A pair of sets of calls with an access path. *)
type callsPairWithPath = callsPair * AccessPath.t option

(* ****************************** Functions ********************************* *)

(** Checks whether pairs of sets of calls are equal. *)
let calls_pairs_eq
  (p1Fst, p1Snd : callsPair) (p2Fst, p2Snd : callsPair) : bool =
  SSet.equal p1Fst p2Fst && SSet.equal p1Snd p2Snd

(* ****************************** Modules *********************************** *)

(** A set of pairs of sets of calls. *)
module CallsPairSet = Set.Make (struct
  type t = callsPair

  let compare (p1 : t) (p2 : t) : int =
    if calls_pairs_eq p1 p2 then 0
    else if Stdlib.compare p1 p2 > 0 then 1 else -1
end)

(** A set of sets of strings. *)
module SSSet = Set.Make (SSet)

(** A set of pairs of sets of calls with an access path. *)
module CallsPairWithPathSet = Set.Make (struct
  type t = callsPairWithPath

  let compare (p1, path1 : t) (p2, path2 : t) : int =
    if calls_pairs_eq p1 p2 && Opt.equal AccessPath.equal path1 path2 then 0
    else if Stdlib.compare p1 p2 > 0 then 1 else -1
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
    then 0 else if Stdlib.compare e1 e2 > 0 then 1 else -1
end)

type t = TSet.t

let initial : t =
  (* An initial abstract state is a set with a single empty element. *)
  TSet.singleton
    { calls= SSet.empty
    ; callsPairs= CallsPairWithPathSet.empty
    ; finalCallsPairs= CallsPairSet.empty }

let pp (fmt : F.formatter) (astate : t) : unit =
  F.pp_print_string fmt "\n";

  let iterator (astateEl : tElement) : unit =
    F.pp_print_string fmt "{\n";

    (* calls *)
    F.fprintf
      fmt "\t{%s};\n" (S.concat (SSet.elements astateEl.calls) ~sep:", ");

    (* callsPairs *)
    F.pp_print_string fmt "\t";
    let lastCallsPair : callsPairWithPath option =
      CallsPairWithPathSet.max_elt_opt astateEl.callsPairs
    in
    let print_calls_pair
      ((pFst, pSnd), path as callsPair : callsPairWithPath) : unit =
      let withoutLock : string = S.concat (SSet.elements pFst) ~sep:", "
      and withLock : string = S.concat (SSet.elements pSnd) ~sep:", " in
      ( match path with
        Some (path : AccessPath.t) ->
          F.fprintf
            fmt "%a: {%s} ( {%s} )" AccessPath.pp path withoutLock withLock

        | None -> F.fprintf fmt "{%s} ( {%s} )" withoutLock withLock
      );

      if not (phys_equal callsPair (Opt.value_exn lastCallsPair)) then
        F.pp_print_string fmt " | "
    in
    CallsPairWithPathSet.iter print_calls_pair astateEl.callsPairs;
    F.pp_print_string fmt ";\n";

    (* finalCallsPairs *)
    F.pp_print_string fmt "\t";
    let lastFinalCallsPair : callsPair option =
      CallsPairSet.max_elt_opt astateEl.finalCallsPairs
    in
    let print_final_calls_pair (pFst, pSnd as callsPair : callsPair) : unit =
      let withoutLock : string = S.concat (SSet.elements pFst) ~sep:", "
      and withLock : string = S.concat (SSet.elements pSnd) ~sep:", " in
      F.fprintf fmt "{%s} ( {%s} )" withoutLock withLock;

      if not (phys_equal callsPair (Opt.value_exn lastFinalCallsPair)) then
        F.pp_print_string fmt " | "
    in
    CallsPairSet.iter print_final_calls_pair astateEl.finalCallsPairs;
    F.pp_print_string fmt ";\n";

    F.pp_print_string fmt "}\n"
  in
  TSet.iter iterator astate;

  F.pp_print_string fmt "\n"

let update_astate_on_function_call (astate : t) (f : string) : t =
  let mapper (astateEl : tElement) : tElement =
      let calls : SSet.t = SSet.add f astateEl.calls
      and callsPairs : CallsPairWithPathSet.t =
        CallsPairWithPathSet.map
          ( fun ((pFst, pSnd), path : callsPairWithPath) : callsPairWithPath ->
            (pFst, SSet.add f pSnd), path )
          astateEl.callsPairs
      in

      (* Update the calls and the calls pairs. *)
      {astateEl with calls= calls; callsPairs= callsPairs}
  in
  TSet.map mapper astate

let update_astate_on_lock (astate : t) (lockPath : AccessPath.t option) : t =
  let mapper (astateEl : tElement) : tElement =
    let callsPairs : CallsPairWithPathSet.t =
      CallsPairWithPathSet.add
        ((astateEl.calls, SSet.empty), lockPath) astateEl.callsPairs
    in

    (* Clear the calls and update the calls pairs. *)
    {astateEl with calls= SSet.empty; callsPairs= callsPairs}
  in
  TSet.map mapper astate

let update_astate_on_unlock (astate : t) (lockPath : AccessPath.t option) : t =
  let mapper (astateEl : tElement) : tElement =
    let finalCallsPairs : CallsPairSet.t ref = ref astateEl.finalCallsPairs in
    let callsPairs : CallsPairWithPathSet.t =
      let filter (p, path : callsPairWithPath) : bool =
        if Opt.equal AccessPath.equal path lockPath then (
          finalCallsPairs := CallsPairSet.add p !finalCallsPairs;
          false
        ) else true
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

    CallsPairWithPathSet.iter
      ( fun (p, _ : callsPairWithPath) : unit ->
        finalCallsPairs := CallsPairSet.add p !finalCallsPairs )
      astateEl.callsPairs;

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
  F.pp_print_string fmt "\n";

  (* atomicFunctions *)
  let lastAtomicFunctions : SSet.t option =
    SSSet.max_elt_opt summary.atomicFunctions
  in
  let print_atomic_functions (atomicFunctions : SSet.t) : unit =
    F.fprintf fmt "{%s}" (S.concat (SSet.elements atomicFunctions) ~sep:", ");

    if not (phys_equal atomicFunctions (Opt.value_exn lastAtomicFunctions)) then
      F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "atomicFunctions: ";
  SSSet.iter print_atomic_functions summary.atomicFunctions;
  F.pp_print_string fmt "\n";

  (* allOccurrences *)
  F.fprintf
    fmt
    "allOccurrences: {%s}\n"
    (S.concat (SSet.elements summary.allOccurrences) ~sep:", ");

  F.pp_print_string fmt "\n"

let update_astate_on_function_call_with_summary
  (astate : t) (summary : summary) : t =
  (* Add all occurrences from a given summary to the calls pairs and to the
     calls of each element of the abstract state. *)
  if SSet.is_empty summary.allOccurrences then astate
  else
    let mapper (astateEl : tElement) : tElement =
      let calls : SSet.t = SSet.union astateEl.calls summary.allOccurrences
      and callsPairs : CallsPairWithPathSet.t =
        CallsPairWithPathSet.map
          ( fun ((pFst, pSnd), path : callsPairWithPath) : callsPairWithPath ->
            (pFst, SSet.union pSnd summary.allOccurrences), path )
          astateEl.callsPairs
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
    let iterator (pFst, pSnd : callsPair) : unit =
      if not (SSet.is_empty pSnd) then
        atomicFunctions := SSSet.add pSnd !atomicFunctions;

      allOccurrences := SSet.union !allOccurrences (SSet.union pFst pSnd)
    in
    CallsPairSet.iter iterator astateEl.finalCallsPairs
  in
  TSet.iter iterator astate;

  {atomicFunctions= !atomicFunctions; allOccurrences= !allOccurrences}

let print_atomic_sets
  (oc : Out_channel.t) (f : string) (summary : summary) : unit =
  if not (SSSet.is_empty summary.atomicFunctions) then (
    Out_channel.fprintf oc "%s: " f;

    let lastAtomicFunctions : SSet.t option =
      SSSet.max_elt_opt summary.atomicFunctions
    in
    let print_atomic_functions (atomicFunctions : SSet.t) : unit =
      Out_channel.fprintf
        oc "{%s}" (S.concat (SSet.elements atomicFunctions) ~sep:", ");

      if
        not (phys_equal atomicFunctions (Opt.value_exn lastAtomicFunctions))
      then Out_channel.output_string oc " "
    in
    SSSet.iter print_atomic_functions summary.atomicFunctions;

    Out_channel.newline oc
  )

(* ****************************** Operators ********************************* *)

(* The lhs is less or equal to the rhs if lhs is a subset of the rhs. *)
let leq ~lhs:(l : t) ~rhs:(r : t) : bool = TSet.subset l r

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
  if i <= Config.atomic_sets_widen_limit then join p n else p
