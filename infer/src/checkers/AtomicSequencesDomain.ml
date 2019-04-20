(** Detection of atomic sequences domain implementation. *)

open! IStd
open! AtomicityUtils

module F = Format
module L = List
module S = String
module OC = Out_channel
module P = Pervasives

(* ****************************** Astate ************************************ *)

(** The set of a string list. *)
module StringListSet = Set.Make (struct
  type t = string list [@@ deriving sexp]

  let compare (e1 : t) (e2 : t) : int =
    if string_lists_eq e1 e2 then 0
    else if P.compare e1 e2 > 0 then 1
    else -1
end)

(** The element of an abstract state. *)
type tElement =
  { firstOccurrences : string list
  ; callSequence : string list
  ; finalCalls : StringListSet.t
  ; isInLock : bool } [@@ deriving sexp]

(** The set of the type tElement is an abstract state. *)
module TSet = Set.Make (struct
  type t = tElement [@@ deriving sexp]

  let compare (e1 : t) (e2 : t) : int =
    if
      string_lists_eq e1.firstOccurrences e2.firstOccurrences
      && string_lists_eq e1.callSequence e2.callSequence
      && StringListSet.equal e1.finalCalls e2.finalCalls
      && phys_equal e1.isInLock e2.isInLock
    then 0
    else if P.compare e1 e2 > 0 then 1
    else -1
end)

type t = TSet.t

let initial : t =
  (* The initial abstract state is a set with a single empty element. *)
  TSet.singleton
    { firstOccurrences : string list= []
    ; callSequence : string list= []
    ; finalCalls : StringListSet.t= StringListSet.empty
    ; isInLock : bool= false }

let pp (fmt : F.formatter) (astate : t) : unit =
  let lastAstateEl : tElement = TSet.max_elt_exn astate in

  let print_first_occurrences (astateEl : tElement) : unit =
    F.fprintf fmt "{%s}" (S.concat astateEl.firstOccurrences ~sep:" ");
    if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "firstOccurrences: ";
  TSet.iter astate ~f:print_first_occurrences;
  F.pp_print_string fmt "\n";

  let print_call_sequence (astateEl : tElement) : unit =
    F.fprintf fmt "{%s}" (S.concat astateEl.callSequence ~sep:" ");
    if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "callSequence: ";
  TSet.iter astate ~f:print_call_sequence;
  F.pp_print_string fmt "\n";

  let print_final_calls (astateEl : tElement) : unit =
    F.pp_print_string fmt "{";

    let lastFinalCallsSequenceOption : (string list) option =
      StringListSet.max_elt astateEl.finalCalls
    in
    let print_final_calls_sequence (sequence : string list) : unit =
      F.pp_print_string fmt (S.concat sequence ~sep:" ");

      match lastFinalCallsSequenceOption with
      | Some (lastFinalCallsSequence : string list) ->
        if not (phys_equal sequence lastFinalCallsSequence) then
          F.pp_print_string fmt " | "

      | None -> ()
    in
    StringListSet.iter astateEl.finalCalls ~f:print_final_calls_sequence;

    F.pp_print_string fmt "}";
    if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "finalCalls: ";
  TSet.iter astate ~f:print_final_calls;
  F.pp_print_string fmt "\n";

  let print_is_in_lock (astateEl : tElement) : unit =
    F.fprintf fmt "%B" astateEl.isInLock;
    if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "isInLock: ";
  TSet.iter astate ~f:print_is_in_lock;
  F.pp_print_string fmt "\n\n"

(** Adss first occurrences to the call sequence. *)
let call_sequence_add_first_occurrences
  (callSequence : string list) (firstOccurrences : string list) : string list =
  let callSequence : string list = callSequence @ firstOccurrences in

  (* If an atomicity sequence in the callSequence is empty, then remove
     the character '(', otherwise close a sequence with adding
     the character ')'. *)
  if s_eq (L.last_exn callSequence) "(" then list_remove_last callSequence
  else callSequence @ [")"]

(** Adds the call sequence to final calls. *)
let final_calls_add_call_sequence
  (finalCalls : StringListSet.t)
  (callSequence : string list)
  (firstOccurrences : string list)
  : StringListSet.t =
  let callSequence : string list =
    call_sequence_add_first_occurrences callSequence firstOccurrences
  in

  (* Add the callSequence to finalCalls only if there is no such
     sequence yet. *)
  StringListSet.add finalCalls callSequence

let update_astate_on_function_call (astate : t) (f : string) : t =
  let mapper (astateEl : tElement) : tElement =
      let firstOccurrences : string list =
        (* Add the function name to first occurrences only if there is no
           such funcion yet. *)
        string_list_add_unique astateEl.firstOccurrences f
      in

      (* Update first occurrences. *)
      {astateEl with firstOccurrences= firstOccurrences}
  in
  TSet.map astate ~f:mapper

let update_astate_on_lock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    (* Ignore a lock call if the abstract state is already in a lock. *)
    if astateEl.isInLock then astateEl
    else
      let callSequence : string list =
        astateEl.callSequence @ astateEl.firstOccurrences @ ["("]
      in

      (* Clear first occurrences, update the call sequence and
         set the 'isInLock'. *)
      { astateEl with
        firstOccurrences= []
      ; callSequence= callSequence
      ; isInLock= true }
  in
  TSet.map astate ~f:mapper

let update_astate_on_unlock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    (* Ignore an unlock call if the abstract state is not in a lock. *)
    if not astateEl.isInLock then astateEl
    else
      let finalCalls : StringListSet.t =
        final_calls_add_call_sequence
          astateEl.finalCalls astateEl.callSequence astateEl.firstOccurrences
      in

      (* Clear first occurrences and the call sequence, update final calls
         and unset the 'isInLock'. *)
      { firstOccurrences= []
      ; callSequence= []
      ; finalCalls= finalCalls
      ; isInLock= false }
  in
  TSet.map astate ~f:mapper

let update_astate_at_the_end_of_function (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    let finalCalls : StringListSet.t =
      (* If the call sequence is empty, add first occurrences to final calls,
         otherwise update the call sequence with first occurrences and add
         it to final calls. *)
      if L.is_empty astateEl.callSequence then
        StringListSet.add astateEl.finalCalls astateEl.firstOccurrences
      else
        final_calls_add_call_sequence
          astateEl.finalCalls astateEl.callSequence astateEl.firstOccurrences
    in

    (* Clear first occurrences and the call sequence, update final calls
       and unset the 'isInLock'. *)
    { firstOccurrences= []
    ; callSequence= []
    ; finalCalls= finalCalls
    ; isInLock= false }
  in
  TSet.map astate ~f:mapper

(* ****************************** Summary *********************************** *)

type summary =
  {atomicSequences : (string list) list; allOccurrences : string list}

let pp_summary (fmt : F.formatter) (summary : summary) : unit =
  let atomicSequencesLength : int = L.length summary.atomicSequences in
  let print_atomic_sequence (i : int) (sequence : string list) : unit =
    F.fprintf fmt "(%s)" (S.concat sequence ~sep:" ");
    if not (phys_equal i (atomicSequencesLength - 1)) then
      F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "atomicSequences: ";
  L.iteri summary.atomicSequences ~f:print_atomic_sequence;
  F.pp_print_string fmt "\n";

  F.fprintf
    fmt "allOccurrences: %s\n\n" (S.concat summary.allOccurrences ~sep:" ")

let update_astate_on_function_call_with_summary
  (astate : t) (summary : summary) : t =
  (* Add all occurrences from the given summary to first occurrences of each
     element of the abstract state. *)
  if L.is_empty summary.allOccurrences then astate
  else
    let mapper (astateEl : tElement) : tElement =
      let firstOccurrences : (string list) ref =
        ref astateEl.firstOccurrences
      in

      L.iter summary.allOccurrences ~f:( fun (f : string) : unit ->
        firstOccurrences := string_list_add_unique !firstOccurrences f );

      {astateEl with firstOccurrences= !firstOccurrences}
    in
    TSet.map astate ~f:mapper

let convert_astate_to_summary (astate : t) : summary =
  (* Derivates atomicity sequences and all occurrences from final calls
     of elements of the abstract state. *)
  let atomicSequences : ((string list) list) ref = ref []
  and allOccurrences : (string list) ref = ref [] in

  let iterator (astateEl : tElement) : unit =
    let atomicSequence : (string list) ref = ref []
    and appendToAtomicSequence : bool ref = ref false in

    let iterator (sequence : string list) : unit =
      let iterator (f : string) : unit =
        if s_eq f "(" then appendToAtomicSequence := true
        else if s_eq f ")" then
        (
          atomicSequences :=
            string_list_list_add_unique !atomicSequences !atomicSequence;
          atomicSequence := [];
          appendToAtomicSequence := false
        )
        else
        (
          allOccurrences := string_list_add_unique !allOccurrences f;
          if !appendToAtomicSequence then
            atomicSequence := !atomicSequence @ [f]
        )
      in
      L.iter sequence ~f:iterator
    in
    StringListSet.iter astateEl.finalCalls ~f:iterator
  in
  TSet.iter astate ~f:iterator;

  {atomicSequences= !atomicSequences; allOccurrences= !allOccurrences}

let print_atomic_sequences
  (oc : OC.t) (f : string) (summary : summary) : unit =
  OC.fprintf oc "%s: " f;
  let atomicSequencesLength : int = L.length summary.atomicSequences in
  let print_atomicity_sequence (i : int) (sequence : string list) : unit =
    OC.fprintf oc "(%s)" (S.concat sequence ~sep:" ");
    if not (phys_equal i (atomicSequencesLength - 1)) then
      OC.output_string oc " "
  in
  L.iteri summary.atomicSequences ~f:print_atomicity_sequence;
  OC.newline oc

(* ****************************** Operators ********************************* *)

let ( <= ) ~lhs:(l : t) ~rhs:(r : t) : bool =
  (* lhs <= rhs if the lhs is subset of the rhs. *)
  if phys_equal l r then true else TSet.is_subset l ~of_:r

let join (astate1 : t) (astate2 : t) : t =
  (* A Union of abstract states. *)
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

let widen ~prev:(p : t) ~next:(n : t) ~num_iters:(_ : int) : t =
  (* Join previous and next abstract states. *)

  (* if P.( <= ) i 2 then join p n else p *)
  join p n
