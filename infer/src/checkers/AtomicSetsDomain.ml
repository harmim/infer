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

(** Pair of sets of calls. Calls without a lock followed by calls with
    a lock. *)
type callsPair = SSet.t * SSet.t

(* ****************************** Functions ********************************* *)

(** Checkes whether pairs of sets of calls are equal. *)
let calls_pairs_eq
  (((p1Fst : SSet.t), (p1Snd : SSet.t)) : callsPair)
  (((p2Fst : SSet.t), (p2Snd : SSet.t)) : callsPair)
  : bool = SSet.equal p1Fst p2Fst && SSet.equal p1Snd p2Snd

(** Empty pair of sets of calls. *)
let emptyCallsPair : callsPair = (SSet.empty, SSet.empty)

(** Checkes whether pair of sets of calls is empty. *)
let calls_pair_empty (p : callsPair) : bool =
  calls_pairs_eq p emptyCallsPair

(* ****************************** Modules *********************************** *)

(** Set of pairs of sets of calls. *)
module CallsPairSet = Set.Make (struct
  type t = callsPair

  let compare (p1 : t) (p2 : t) : int =
    if calls_pairs_eq p1 p2 then 0 else if P.compare p1 p2 > 0 then 1 else -1
end)

(** Set of sets of strings. *)
module SSSet = Set.Make (SSet)

(* ****************************** Astate ************************************ *)

(** Element of an abstract state. *)
type tElement =
  {callsPair: callsPair; finalCalls: CallsPairSet.t; isInLock: bool}

(** Set of types tElement is an abstract state. *)
module TSet = Set.Make (struct
  type t = tElement

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
    { callsPair= emptyCallsPair
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
      CallsPairSet.max_elt_opt astateEl.finalCalls
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
    CallsPairSet.iter print_final_calls_pairs astateEl.finalCalls;
    F.pp_print_string fmt ";\n";

    (* isInLock *)
    F.fprintf fmt "%B;\n" astateEl.isInLock;

    F.pp_print_string fmt "}\n"
  in
  TSet.iter iterator astate;

  F.pp_print_string fmt "\n\n"

let update_astate_on_function_call (astate : t) (f : string) : t =
  let mapper (astateEl : tElement) : tElement =
      (* Add a function name to the calls pair. *)
      let callsPair : callsPair =
        if astateEl.isInLock then
          (P.fst astateEl.callsPair, SSet.add f (P.snd astateEl.callsPair))
        else (SSet.add f (P.fst astateEl.callsPair), SSet.empty)
      in

      (* Update the calls pair. *)
      {astateEl with callsPair= callsPair}
  in
  TSet.map mapper astate

let update_astate_on_lock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    (* Ignore a lock call if an abstract state is already in a lock. *)
    if astateEl.isInLock then astateEl
    else {astateEl with isInLock= true} (* Set 'isInLock'. *)
  in
  TSet.map mapper astate

let update_astate_on_unlock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    (* Ignore an unlock call if an abstract state is not in a lock. *)
    if not astateEl.isInLock then astateEl
    else
      let finalCalls : CallsPairSet.t =
        if calls_pair_empty astateEl.callsPair then astateEl.finalCalls
        else CallsPairSet.add astateEl.callsPair astateEl.finalCalls
      in

      (* Clear the calls pair, update the final calls and unset 'isInLock'. *)
      { callsPair= emptyCallsPair
      ; finalCalls= finalCalls
      ; isInLock= false }
  in
  TSet.map mapper astate

let update_astate_at_the_end_of_function (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    let finalCalls : CallsPairSet.t =
      if calls_pair_empty astateEl.callsPair then astateEl.finalCalls
      else CallsPairSet.add astateEl.callsPair astateEl.finalCalls
    in

    (* Clear the calls pair, update the final calls and unset 'isInLock'. *)
    { callsPair= emptyCallsPair
    ; finalCalls= finalCalls
    ; isInLock= false }
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
  (* Add all occurrences from a given summary to the calls pairs of each
     element of the abstract state. *)
  if SSet.is_empty summary.allOccurrences then astate
  else
    let mapper (astateEl : tElement) : tElement =
      let callsPair : callsPair ref = ref astateEl.callsPair in

      if astateEl.isInLock then
        callsPair :=
          ( P.fst !callsPair
          , SSet.union (P.snd !callsPair) summary.allOccurrences )
      else
        callsPair :=
          (SSet.union (P.fst !callsPair) summary.allOccurrences, SSet.empty);

      {astateEl with callsPair= !callsPair}
    in
    TSet.map mapper astate

let convert_astate_to_summary (astate : t) : summary =
  (* Derivates atomic functions and all occurrences from the final calls
     of elements of the abstract state. *)
  let atomicFunctions : SSSet.t ref = ref SSSet.empty
  and allOccurrences : SSet.t ref = ref SSet.empty in

  let iterator (astateEl : tElement) : unit =
    let iterator (callsPair : callsPair) : unit =
      if not (SSet.is_empty (P.snd callsPair)) then
        atomicFunctions := SSSet.add (P.snd callsPair) !atomicFunctions;

      allOccurrences :=
        SSet.union
          !allOccurrences (SSet.union (P.fst callsPair) (P.snd callsPair))
    in
    CallsPairSet.iter iterator astateEl.finalCalls
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

(* lhs is less or equal to rhs if lhs is subset of rhs. *)
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
  (* Join previous and next abstract states. (just 2 iterations
     for better scalability) TODO: dynamic set number of iterations *)
  if P.( <= ) i 2 then join p n else p
