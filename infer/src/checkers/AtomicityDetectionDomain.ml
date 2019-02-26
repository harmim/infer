(** Atomicity detection domain implementation. *)

open! IStd
module F = Format
module L = List
module S = String
module OC = Out_channel
module P = Pervasives

(* ****************************** Functions ********************************* *)

(** Checks whether strings are equal. *)
let strings_equal (s1 : string) (s2 : string) : bool =
  phys_equal (S.compare s1 s2) 0

(** Checks whether lists are equal. *)
let lists_equal
  (l1 : 'a list) (l2 : 'a list) (cmp : ('a -> 'a -> bool)) : bool =
  (* A length of lists and theirs elements must be equal. *)
  if not (phys_equal (L.length l1) (L.length l2)) then false
  else
  (
    let eq : bool ref = ref true in

    L.iter2_exn
      l1 l2 ~f:(fun (e1 : 'a) (e2 : 'a) : unit ->
        if not (cmp e1 e2) then eq := false);

    !eq
  )

(** Checks whether string lists are equal. *)
let string_lists_equal (l1 : string list) (l2 : string list) : bool =
  lists_equal l1 l2 strings_equal

(** Removes a last element from the list. *)
let list_remove_last (l : 'a list) : 'a list =
  let lLength : int = L.length l in

  L.filteri
    l ~f:(fun (i : int) (_ : 'a) : bool -> not (phys_equal i (lLength - 1)))

(** Adds the element to the list without duplicities. *)
let list_add_unique (l : 'a list) (e : 'a) (eq : ('a -> 'a -> bool)) : 'a list =
  if L.mem l e ~equal:eq then l else l @ [e]

(** Adds the string element to the list without duplicities. *)
let string_list_add_unique (l : string list) (s : string) : string list =
  list_add_unique l s strings_equal

(** Adds the string list element to the list without duplicities. *)
let string_list_list_add_unique
  (ll : (string list) list) (l : string list) : (string list) list =
  list_add_unique ll l string_lists_equal

let is_lock (f : string) : bool = strings_equal f "pthread_mutex_lock"

let is_unlock (f : string) : bool = strings_equal f "pthread_mutex_unlock"

(* ****************************** Astate ************************************ *)

(** The set of a string list. *)
module StringListSet = Set.Make (struct
  type t = string list [@@ deriving sexp]

  let compare (e1 : t) (e2 : t) : int =
    if string_lists_equal e1 e2 then 0
    else if P.compare e1 e2 > 0 then 1 else -1
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
      string_lists_equal e1.firstOccurrences e2.firstOccurrences
      && string_lists_equal e1.callSequence e2.callSequence
      && StringListSet.equal e1.finalCalls e2.finalCalls
    then 0
    else if P.compare e1 e2 > 0 then 1 else -1
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
  let print_final_calls (astateEl : tElement) : unit =
    if not (StringListSet.is_empty astateEl.finalCalls) then
    (
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
    )
  in
  F.pp_print_string fmt "finalCalls: ";
  TSet.iter astate ~f:print_final_calls;
  F.pp_print_string fmt "\n"

  (* ; let print_first_occurrences (astateEl : tElement) : unit =
    (* if not (L.is_empty astateEl.firstOccurrences) then *)
    (
      F.pp_print_string
        fmt ("{" ^ (S.concat astateEl.firstOccurrences ~sep:" ") ^ "}");
      if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
    )
  in
  F.pp_print_string fmt "firstOccurrences: ";
  TSet.iter astate ~f:print_first_occurrences;
  F.pp_print_string fmt "\n" *)

  (* ; let print_call_sequence (astateEl : tElement) : unit =
    (* if not (L.is_empty astateEl.callSequence) then *)
    (
      F.pp_print_string
        fmt ("{" ^ (S.concat astateEl.callSequence ~sep:" ") ^ "}");
      if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
    )
  in
  F.pp_print_string fmt "callSequence: ";
  TSet.iter astate ~f:print_call_sequence;
  F.pp_print_string fmt "\n\n" *)

(** Adss first occurrences to the call sequence. *)
let call_sequence_add_first_occurrences
  (callSequence : string list) (firstOccurrences : string list) : string list =
  let callSequence : string list = callSequence @ firstOccurrences in

  (* If an atomicity sequence in the callSequence is empty, then remove
     the character '(', otherwise close a sequence with adding
     the character ')'. *)
  if strings_equal (L.last_exn callSequence) "(" then
    list_remove_last callSequence
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
  {atomicitySequences : (string list) list; allOccurrences : string list}

let pp_summary (fmt : F.formatter) (summary : summary) : unit =
  let atomicitySequencesLength : int = L.length summary.atomicitySequences in
  let print_atomicity_sequence (i : int) (sequence : string list) : unit =
    F.pp_print_string fmt ("(" ^ (S.concat sequence ~sep:" ") ^ ")");
    if not (phys_equal i (atomicitySequencesLength - 1)) then
      F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "atomicitySequences: ";
  L.iteri summary.atomicitySequences ~f:print_atomicity_sequence;
  F.pp_print_string fmt "\n";

  F.pp_print_string
    fmt
    ("allOccurrences: " ^ (S.concat summary.allOccurrences ~sep:" ") ^ "\n")

let update_astate_on_function_call_with_summary
  (astate : t) (summary : summary) : t =
  (* Add all occurrences from the given summary to first occurrences of each
     element of the abstract state. *)
  if L.is_empty summary.allOccurrences then astate
  else
    let mapper (astateEl : tElement) : tElement =
      let firstOccurrences : string list ref = ref astateEl.firstOccurrences in

      L.iter summary.allOccurrences ~f:(fun (f : string) : unit ->
        firstOccurrences := string_list_add_unique !firstOccurrences f);

      {astateEl with firstOccurrences= !firstOccurrences}
    in
    TSet.map astate ~f:mapper

let convert_astate_to_summary (astate : t) : summary =
  (* Derivates atomicity sequences and all occurrences from final calls
     of elements of the abstract state. *)
  let atomicitySequences : (string list) list ref = ref []
  and allOccurrences : string list ref = ref [] in

  let iterator (astateEl : tElement) : unit =
    let atomicitySequence : string list ref = ref []
    and appendToAtomicitySequence : bool ref = ref false in

    let iterator (sequence : string list) : unit =
      let iterator (f : string) : unit =
        if strings_equal f "(" then appendToAtomicitySequence := true
        else if strings_equal f ")" then
        (
          atomicitySequences :=
            string_list_list_add_unique !atomicitySequences !atomicitySequence;
          atomicitySequence := [];
          appendToAtomicitySequence := false
        )
        else
        (
          allOccurrences := string_list_add_unique !allOccurrences f;
          if !appendToAtomicitySequence then
            atomicitySequence := !atomicitySequence @ [f]
        )
      in
      L.iter sequence ~f:iterator
    in
    StringListSet.iter astateEl.finalCalls ~f:iterator
  in
  TSet.iter astate ~f:iterator;

  {atomicitySequences= !atomicitySequences; allOccurrences= !allOccurrences}

(* ****************************** Reporting ********************************* *)

let report (oc : OC.t) (f : string) (summary : summary) : unit =
  OC.output_string oc (f ^ ": ");
  let atomicitySequencesLength : int = L.length summary.atomicitySequences in
  let print_atomicity_sequence (i : int) (sequence : string list) : unit =
    OC.output_string oc ("(" ^ (S.concat sequence ~sep:" ") ^ ")");
    if not (phys_equal i (atomicitySequencesLength - 1)) then
      OC.output_string oc " "
  in
  L.iteri summary.atomicitySequences ~f:print_atomicity_sequence;
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

  (* F.fprintf F.std_formatter "\nJoin:\n"; *)
  (* F.fprintf F.std_formatter "\n1:\n"; *)
  (* pp F.std_formatter astate1; *)
  (* F.fprintf F.std_formatter "\n2:\n"; *)
  (* pp F.std_formatter astate2; *)
  (* F.fprintf F.std_formatter "\nresult:\n"; *)
  (* pp F.std_formatter result; *)
  (* F.fprintf F.std_formatter "\n\n"; *)

  (* result *)

let widen ~prev:(p : t) ~next:(n : t) ~num_iters:(i : int) : t =
  (* Join previous and next abstract states. *)
  (* let isInLock : bool ref = ref false in
  let iterator (astateEl : tElement) : unit =
    if astateEl.isInLock then isInLock := true
  in
  TSet.iter n ~f:iterator; *)

  (* if !isInLock || P.( <= ) i 2 then join p n else p *)
  if P.( <= ) i 2 then join p n else p
