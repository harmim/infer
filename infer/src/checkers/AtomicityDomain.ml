open! IStd
module F = Format
module L = List
module S = String

(******************************** Functions ***********************************)

let strings_equal (s1 : string) (s2 : string) : bool =
  phys_equal (S.compare s1 s2) 0

let lists_equal
  (l1 : 'a list) (l2 : 'b list) (cmp : ('a -> 'b -> bool)) : bool =
  if not (phys_equal (L.length l1) (L.length l2)) then false
  else
  (
    let eq : bool ref = ref true in

    let iterator (e1 : 'a) (e2 : 'b) : unit =
      if not (cmp e1 e2) then eq := false
    in
    L.iter2_exn l1 l2 ~f:iterator;

    !eq
  )

let string_lists_equal (l1 : string list) (l2 : string list) : bool =
  lists_equal l1 l2 strings_equal

let list_remove_last (l : 'a list) : 'a list =
  let lLength : int = L.length l in

  let filter (i : int) (_ : 'a) : bool = not (phys_equal i (lLength - 1)) in
  L.filteri l ~f:filter

let list_is_sublist
  (l1 : 'a list) (l2 : 'b list) (cmp : ('a -> 'b -> bool)) : bool =
  let l1Length : int = L.length l1 and l2Length : int = L.length l2 in

  if l1Length > l2Length then false
  else
  (
    let i : int ref = ref 0 and j : int ref = ref 0 in

    while !i < l2Length && not (phys_equal !j l1Length) do
      if cmp (L.nth_exn l2 !i) (L.nth_exn l1 !j) then j := !j + 1
      else
      (
        i := !i - !j;
        j := 0
      );

      i := !i + 1
    done;

    phys_equal !j l1Length
  )

let string_list_is_sublist (l1 : string list) (l2 : string list) : bool =
  list_is_sublist l1 l2 strings_equal

let list_add_unique (l : 'a list) (e : 'a) (eq : ('a -> 'a -> bool)) : 'a list =
  if L.mem l e ~equal:eq then l else l @ [e]

let string_list_add_unique (l : string list) (s : string) : string list =
  list_add_unique l s strings_equal

let is_lock (f : string) : bool = strings_equal f "pthread_mutex_lock"

let is_unlock (f : string) : bool = strings_equal f "pthread_mutex_unlock"

(******************************** Astate **************************************)

type tElement =
  { firstOccurrences : string list
  ; callSequence : string list
  ; finalCalls : string list
  ; isInLock : bool } [@@ deriving sexp]

module TSet = Set.Make (struct
  type t = tElement [@@ deriving sexp]

  let compare (e1 : t) (e2 : t) : int =
    if
      string_lists_equal e1.firstOccurrences e2.firstOccurrences
      && string_lists_equal e1.callSequence e2.callSequence
      && string_lists_equal e1.finalCalls e2.finalCalls
    then 0
    else
      let e1Length : int =
        L.length e1.firstOccurrences +
        L.length e1.callSequence +
        L.length e1.finalCalls
      and e2Length : int =
        L.length e2.firstOccurrences +
        L.length e2.callSequence +
        L.length e2.finalCalls
      in

      if e1Length >= e2Length then 1 else -1
end)

type t = TSet.t

let initial : t =
  TSet.singleton
    { firstOccurrences : string list= []
    ; callSequence : string list= []
    ; finalCalls : string list= []
    ; isInLock : bool= false }

let pp (fmt : F.formatter) (astate : t) : unit =
  let lastAstateEl : tElement = TSet.max_elt_exn astate in
  let print_final_calls (astateEl : tElement) : unit =
    if not (L.is_empty astateEl.finalCalls) then
    (
      F.pp_print_string
        fmt ("{" ^ (S.concat astateEl.finalCalls ~sep:" ") ^ "}");
      if not (phys_equal astateEl lastAstateEl) then F.pp_print_string fmt " "
    )
  in
  F.pp_print_string fmt "finalCalls: ";
  TSet.iter astate ~f:print_final_calls;
  F.pp_print_string fmt "\n"

let call_sequence_add_first_occurrences
  (callSequence : string list) (firstOccurrences : string list) : string list =
  let callSequence : string list = callSequence @ firstOccurrences in

  if strings_equal (L.last_exn callSequence) "(" then
    list_remove_last callSequence
  else callSequence @ [")"]

let final_calls_add_call_sequence
  (finalCalls : string list)
  (callSequence : string list)
  (firstOccurrences : string list)
  : string list =
  let callSequence : string list =
    call_sequence_add_first_occurrences callSequence firstOccurrences
  in

  if string_list_is_sublist callSequence finalCalls then finalCalls
  else finalCalls @ callSequence

let update_astate_on_function_call (astate : t) (f : string) : t =
  let mapper (astateEl : tElement) : tElement =
      let firstOccurrences : string list =
        string_list_add_unique astateEl.firstOccurrences f
      in

      {astateEl with firstOccurrences= firstOccurrences}
  in
  TSet.map astate ~f:mapper

let update_astate_on_lock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    if astateEl.isInLock then astateEl
    else
      let callSequence : string list =
        astateEl.callSequence @ astateEl.firstOccurrences @ ["("]
      in

      { astateEl with
        firstOccurrences= []
      ; callSequence= callSequence
      ; isInLock= true }
  in
  TSet.map astate ~f:mapper

let update_astate_on_unlock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    if not astateEl.isInLock then astateEl
    else
      let finalCalls : string list =
        final_calls_add_call_sequence
          astateEl.finalCalls astateEl.callSequence astateEl.firstOccurrences
      in

      { firstOccurrences= []
      ; callSequence= []
      ; finalCalls= finalCalls
      ; isInLock= false }
  in
  TSet.map astate ~f:mapper

let update_astate_at_the_end_of_function (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    let finalCalls : string list =
      if L.is_empty astateEl.callSequence then
        astateEl.finalCalls @ astateEl.firstOccurrences
      else
        final_calls_add_call_sequence
          astateEl.finalCalls astateEl.callSequence astateEl.firstOccurrences
    in

    { firstOccurrences= []
    ; callSequence= []
    ; finalCalls= finalCalls
    ; isInLock= false }
  in
  TSet.map astate ~f:mapper

(******************************** Summary *************************************)

type summary =
  {atomicitySequences : (string list) list; allOccurrences : string list}

let pp_summary (fmt : F.formatter) (summary : summary) : unit =
  let atomicitySequencesLength : int = L.length summary.atomicitySequences in
  let print_atomicity_sequences (i : int) (sequence : string list) : unit =
    F.pp_print_string fmt ("(" ^ (S.concat sequence ~sep:" ") ^ ")");
    if not (phys_equal i (atomicitySequencesLength - 1)) then
      F.pp_print_string fmt " "
  in
  F.pp_print_string fmt "atomicitySequences: ";
  L.iteri summary.atomicitySequences ~f:print_atomicity_sequences;
  F.pp_print_string fmt "\n";

  F.pp_print_string
    fmt
    ( "allOccurrences: " ^ (S.concat summary.allOccurrences ~sep:" ") ^ "\n" )

let update_astate_on_function_call_with_summary
  (astate : t) (summary : summary) : t =
  if L.is_empty summary.allOccurrences then astate
  else
    let mapper (astateEl : tElement) : tElement =
      let firstOccurrences : string list ref = ref astateEl.firstOccurrences in

      let iterator (f : string) : unit =
        firstOccurrences := string_list_add_unique !firstOccurrences f
      in
      L.iter summary.allOccurrences ~f:iterator;

      {astateEl with firstOccurrences= !firstOccurrences}
    in
    TSet.map astate ~f:mapper

let convert_astate_to_summary (astate : t) : summary =
  let atomicitySequences : (string list) list ref = ref []
  and allOccurrences : string list ref = ref [] in

  let iterator (astateEl : tElement) : unit =
    let atomicitySequence : string list ref = ref []
    and appendToAtomicitySequence : bool ref = ref false in

    let iterator (s : string) : unit =
      if strings_equal s "(" then appendToAtomicitySequence := true
      else if strings_equal s ")" then
      (
        atomicitySequences :=
          list_add_unique
            !atomicitySequences !atomicitySequence string_lists_equal;
        atomicitySequence := [];
        appendToAtomicitySequence := false
      )
      else
      (
        allOccurrences := string_list_add_unique !allOccurrences s;
        if !appendToAtomicitySequence then
          atomicitySequence := !atomicitySequence @ [s]
      )
    in
    L.iter astateEl.finalCalls ~f:iterator
  in
  TSet.iter astate ~f:iterator;

  {atomicitySequences= !atomicitySequences; allOccurrences= !allOccurrences}

(******************************** Operators ***********************************)

let ( <= ) ~lhs:(l : t) ~rhs:(r : t) : bool =
  if phys_equal l r then true else TSet.is_subset l ~of_:r

let join (astate1 : t) (astate2 : t) : t =
  if phys_equal astate1 astate2 then astate1
  else if TSet.is_empty astate1 then astate2
  else if TSet.is_empty astate2 then astate1
  else TSet.union astate1 astate2

let widen ~prev:(p : t) ~next:(n : t) ~num_iters:(_ : int) : t =
  join p n
