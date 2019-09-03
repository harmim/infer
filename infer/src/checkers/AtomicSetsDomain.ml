(** Detection of atomic sets domain implementation. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd
open! AtomicityUtils

module F = Format
module L = List
module OC = Out_channel
module P = Pervasives
module S = String

(* ****************************** Types ************************************* *)

(** Set of strings. *)
module SSet = Set.Make (String)

(** Pair of sets of calls. Calls without a lock followed by calls with
    a lock. *)
type callsPair = SSet.t * SSet.t
[@@ deriving sexp]

(* ****************************** Functions ********************************* *)

(** Checkes whether pairs of sets of calls are equal. *)
let calls_pairs_eq
  (((p1Fst : SSet.t), (p1Snd : SSet.t)) : callsPair)
  (((p2Fst : SSet.t), (p2Snd : SSet.t)) : callsPair)
  : bool = SSet.equal p1Fst p2Fst && SSet.equal p1Snd p2Snd

(** Checkes whether pair of sets of calls is empty. *)
let calls_pair_empty (p : callsPair) : bool =
  calls_pairs_eq p (SSet.empty, SSet.empty)

(* ****************************** Modules *********************************** *)

(** Set of pairs of sets of calls. *)
module CallsPairSet = Set.Make (struct
  type t = callsPair
  [@@ deriving sexp]

  let compare (p1 : t) (p2 : t) : int =
    if calls_pairs_eq p1 p2 then 0 else if P.compare p1 p2 > 0 then 1 else -1
end)

(* ****************************** Astate ************************************ *)

(** Element of an abstract state. *)
type tElement =
  {callsPair : callsPair; finalCalls : CallsPairSet.t; isInLock : bool}
[@@ deriving sexp]

(** Set of types tElement is an abstract state. *)
module TSet = Set.Make (struct
  type t = tElement
  [@@ deriving sexp]

  let compare (e1 : t) (e2 : t) : int =
    if
      calls_pairs_eq e1.callsPair e2.callsPair
      && CallsPairSet.equal e1.finalCalls e2.finalCalls
      && phys_equal e1.isInLock e2.isInLock
    then 0
    else if P.compare e1 e2 > 0 then 1
    else -1
end)

type t = TSet.t

let initial : t =
  (* Initial abstract state is a set with a single empty element. *)
  TSet.singleton
    { callsPair= (SSet.empty, SSet.empty)
    ; finalCalls= CallsPairSet.empty
    ; isInLock= false }

let pp (fmt : F.formatter) (astate : t) : unit =
  let iterator (astateEl : tElement) : unit =
    F.pp_print_string fmt "{\n";

    (* callsPair *)
    let withoutLock : string =
      S.concat (SSet.elements (P.fst astateEl.callsPair)) ~sep:", "
    and withLock : string =
      S.concat (SSet.elements (P.snd astateEl.callsPair)) ~sep:", "
    in
    F.fprintf fmt "{%s} ( {%s} );\n" withoutLock withLock;

    (* finalCalls *)
    let lastFinalCallsPairOption : callsPair option =
      CallsPairSet.max_elt astateEl.finalCalls
    in
    let print_final_calls_pairs (callsPair : callsPair) : unit =
      let withoutLock : string =
        S.concat (SSet.elements (P.fst callsPair)) ~sep:", "
      and withLock : string =
        S.concat (SSet.elements (P.snd callsPair)) ~sep:", "
      in
      F.fprintf fmt "{%s} ( {%s} )" withoutLock withLock;

      match lastFinalCallsPairOption with
      | Some (lastFinalCallsPair : callsPair) ->
        if not (phys_equal callsPair lastFinalCallsPair) then
          F.pp_print_string fmt " | "

      | None -> ()
    in
    CallsPairSet.iter astateEl.finalCalls ~f:print_final_calls_pairs;
    F.pp_print_string fmt ";\n";

    (* isInLock *)
    F.fprintf fmt "%B;\n" astateEl.isInLock;

    F.pp_print_string fmt "}\n"
  in
  TSet.iter astate ~f:iterator;

  F.pp_print_string fmt "\n\n"

let update_astate_on_function_call (astate : t) (f : string) : t =
  let mapper (astateEl : tElement) : tElement =
      (* Add a function name to the calls pair. *)
      let callsPair : callsPair =
        if astateEl.isInLock then
          (P.fst astateEl.callsPair, SSet.add (P.snd astateEl.callsPair) f)
        else (SSet.add (P.fst astateEl.callsPair) f, SSet.empty)
      in

      (* Update the calls pair. *)
      {astateEl with callsPair= callsPair}
  in
  TSet.map astate ~f:mapper

let update_astate_on_lock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    (* Ignore a lock call if an abstract state is already in a lock. *)
    if astateEl.isInLock then astateEl
    else {astateEl with isInLock= true} (* Set 'isInLock'. *)
  in
  TSet.map astate ~f:mapper

let update_astate_on_unlock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    (* Ignore an unlock call if an abstract state is not in a lock. *)
    if not astateEl.isInLock then astateEl
    else
      let finalCalls : CallsPairSet.t =
        if calls_pair_empty astateEl.callsPair then astateEl.finalCalls
        else CallsPairSet.add astateEl.finalCalls astateEl.callsPair
      in

      (* Clear the calls pair, update the final calls and unset 'isInLock'. *)
      { callsPair= (SSet.empty, SSet.empty)
      ; finalCalls= finalCalls
      ; isInLock= false }
  in
  TSet.map astate ~f:mapper

let update_astate_at_the_end_of_function (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    let finalCalls : CallsPairSet.t =
      if calls_pair_empty astateEl.callsPair then astateEl.finalCalls
      else CallsPairSet.add astateEl.finalCalls astateEl.callsPair
    in

    (* Clear the calls pair, update the final calls and unset 'isInLock'. *)
    { callsPair= (SSet.empty, SSet.empty)
    ; finalCalls= finalCalls
    ; isInLock= false }
  in
  TSet.map astate ~f:mapper

(* ****************************** Summary *********************************** *)

type summary =
  {atomicFunctions : (string list) list; allOccurrences : string list}

let pp_summary (fmt : F.formatter) (summary : summary) : unit =
  let atomicFunctionsLength : int = L.length summary.atomicFunctions in
  let print_atomic_functions (i : int) (functionsList : string list) : unit =
    F.fprintf fmt "{%s}" (S.concat functionsList ~sep:", ");
    if not (phys_equal i (atomicFunctionsLength - 1)) then
      F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "atomicFunctions: ";
  L.iteri summary.atomicFunctions ~f:print_atomic_functions;
  F.pp_print_string fmt "\n";

  F.fprintf
    fmt "allOccurrences: {%s}\n\n\n" (S.concat summary.allOccurrences ~sep:", ")

let update_astate_on_function_call_with_summary
  (astate : t) (summary : summary) : t =
  (* Add all occurrences from a given summary to the calls pairs of each
     element of the abstract state. *)
  if L.is_empty summary.allOccurrences then astate
  else
    let mapper (astateEl : tElement) : tElement =
      let callsPair : callsPair ref = ref astateEl.callsPair in

      let iterator (f : string) : unit =
        if astateEl.isInLock then
          callsPair := (P.fst !callsPair, SSet.add (P.snd !callsPair) f)
        else callsPair := (SSet.add (P.fst !callsPair) f, P.snd !callsPair)
      in
      L.iter summary.allOccurrences ~f:iterator;

      {astateEl with callsPair= !callsPair}
    in
    TSet.map astate ~f:mapper

let convert_astate_to_summary (astate : t) : summary =
  (* Derivates atomic functions and all occurrences from the final calls
     of elements of the abstract state. *)
  let atomicFunctions : ((string list) list) ref = ref []
  and allOccurrences : (string list) ref = ref [] in

  let iterator (astateEl : tElement) : unit =
    let iterator (callsPair : callsPair) : unit =
      if not (SSet.is_empty (P.snd callsPair)) then
      (
        let atomicFunctionsList : string list =
          SSet.elements (P.snd callsPair)
        in
        atomicFunctions :=
          string_list_list_add_unique !atomicFunctions atomicFunctionsList
      );

      SSet.iter (P.fst callsPair) ~f:( fun (f : string) : unit ->
        allOccurrences := string_list_add_unique !allOccurrences f; );
      SSet.iter (P.snd callsPair) ~f:( fun (f : string) : unit ->
        allOccurrences := string_list_add_unique !allOccurrences f; )
    in
    CallsPairSet.iter astateEl.finalCalls ~f:iterator
  in
  TSet.iter astate ~f:iterator;

  {atomicFunctions= !atomicFunctions; allOccurrences= !allOccurrences}

let print_atomic_sets (oc : OC.t) (f : string) (summary : summary) : unit =
  OC.fprintf oc "%s: " f;

  let atomicFunctionsLength : int = L.length summary.atomicFunctions in
  let print_atomic_functions (i : int) (functionsList : string list) : unit =
    OC.fprintf oc "{%s}" (S.concat functionsList ~sep:", ");
    if not (phys_equal i (atomicFunctionsLength - 1)) then
      OC.output_string oc " "
  in
  L.iteri summary.atomicFunctions ~f:print_atomic_functions;

  OC.newline oc

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

let widen ~prev:(p : t) ~next:(n : t) ~num_iters:(i : int) : t =
  (* Join previous and next abstract states. (just 10 iterations
     for better scalability) *)
  if P.( <= ) i 10 then join p n else p
