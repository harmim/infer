open! IStd
module F = Format

(******************************** Functions ***********************************)

let strings_equal (s1 : string) (s2 : string) : bool =
  phys_equal (String.compare s1 s2) 0

let lists_equal (l1 : string list) (l2 : string list) : bool =
  let mem_all (l1 : string list) (l2 : string list) : bool =
    let memAll : bool ref = ref true in

    let iterator (s : string) : unit =
      if not (List.mem l2 s ~equal:strings_equal) then memAll := false
    in
    List.iter l1 ~f:iterator;

    !memAll
  in

  phys_equal (List.length l1) (List.length l2)
  && mem_all l1 l2
  && mem_all l2 l1

let print_string_with_space (fmt : F.formatter) (s : string) : unit =
  F.fprintf fmt "%s " s

let is_lock (fn : string) : bool =
  phys_equal (String.compare fn "pthread_mutex_lock") 0

let is_unlock (fn : string) : bool =
  phys_equal (String.compare fn "pthread_mutex_unlock") 0

(******************************** Astate **************************************)

type tElement =
  { firstOccurrences : string list
  ; callSequence : string list
  ; finalCalls : string list } [@@ deriving sexp]

module TSet = Set.Make (struct
  type t = tElement [@@ deriving sexp]

  let compare (e1 : t) (e2 : t) : int =
    if
      lists_equal e1.firstOccurrences e2.firstOccurrences
      && lists_equal e1.callSequence e2.callSequence
      && lists_equal e1.finalCalls e2.finalCalls
    then 0
    else
      let e1Length : int =
        List.length e1.firstOccurrences +
        List.length e1.callSequence +
        List.length e1.finalCalls
      in
      let e2Length : int =
        List.length e2.firstOccurrences +
        List.length e2.callSequence +
        List.length e2.finalCalls
      in

      if e1Length >= e2Length then 1 else -1
end)

type t = TSet.t

let initial : t =
  TSet.singleton
    { firstOccurrences : string list= []
    ; callSequence : string list= []
    ; finalCalls : string list= [] }

let pp (fmt : F.formatter) (astate : t) : unit =
  let print_final_calls (astateEl : tElement) : unit =
    if not (List.is_empty astateEl.finalCalls) then
    (
      F.fprintf fmt "{ ";
      List.iter astateEl.finalCalls ~f:(print_string_with_space fmt);
      F.fprintf fmt "} "
    )
  in

  F.fprintf fmt "finalCalls: ";
  TSet.iter astate ~f:print_final_calls;
  F.fprintf fmt "\n"

let final_calls_add_call_sequence
  (finalCalls : string list) (callSequence : string list) : string list =
  let isCallSequenceInFinalCalls : bool =
    let finalCallsLength : int = List.length finalCalls in
    let callSequenceLength : int = List.length callSequence in

    if callSequenceLength > finalCallsLength then false
    else
    (
      let i : int ref = ref 0 in
      let j : int ref = ref 0 in

      while !i < finalCallsLength && not (phys_equal !j callSequenceLength) do
        let comparedElements : int =
          String.compare
            (List.nth_exn finalCalls !i) (List.nth_exn callSequence !j)
        in

        if phys_equal comparedElements 0 then j := !j + 1
        else
        (
          i := !i - !j;
          j := 0
        );

        i := !i + 1
      done;

      phys_equal !j callSequenceLength
    )
  in

  if isCallSequenceInFinalCalls then finalCalls else finalCalls @ callSequence

let update_astate_on_function_call (astate : t) (fn : string) : t =
  let mapper (astateEl : tElement) : tElement =
    let firstOccurrences : string list =
      if List.mem astateEl.firstOccurrences fn ~equal:strings_equal then
        astateEl.firstOccurrences
      else astateEl.firstOccurrences @ [fn]
    in

    {astateEl with firstOccurrences= firstOccurrences}
  in
  TSet.map astate ~f:mapper

let update_astate_on_lock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    let callSequence : string list =
      astateEl.callSequence @ astateEl.firstOccurrences @ ["("]
    in

    {astateEl with firstOccurrences= []; callSequence= callSequence}
  in
  TSet.map astate ~f:mapper

let update_astate_on_unlock (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    if List.is_empty astateEl.callSequence then astateEl
    else if List.is_empty astateEl.firstOccurrences then
      {astateEl with callSequence= []}
    else
      let callSequence : string list =
        astateEl.callSequence @ astateEl.firstOccurrences @ [")"]
      in
      let finalCalls : string list =
        final_calls_add_call_sequence astateEl.finalCalls callSequence
      in

      {firstOccurrences= []; callSequence= []; finalCalls= finalCalls}
  in
  TSet.map astate ~f:mapper

let update_astate_at_the_end_of_function (astate : t) : t =
  let mapper (astateEl : tElement) : tElement =
    let finalCalls : string list =
      if List.is_empty astateEl.callSequence then
        astateEl.finalCalls @ astateEl.firstOccurrences
      else if List.is_empty astateEl.firstOccurrences then astateEl.finalCalls
      else
        let callSequence : string list =
          astateEl.callSequence @ astateEl.firstOccurrences @ [")"]
        in

        final_calls_add_call_sequence astateEl.finalCalls callSequence
    in

    {firstOccurrences= []; callSequence= []; finalCalls= finalCalls}
  in
  TSet.map astate ~f:mapper

(******************************** Summary *************************************)

type summary = {atomicitySequences : (string list) list}

let pp_summary (fmt : F.formatter) (summary : summary) : unit =
  let print_atomicity_sequences (sequence : string list) : unit =
    F.fprintf fmt "( ";
    List.iter sequence ~f:(print_string_with_space fmt);
    F.fprintf fmt ") "
  in

  F.fprintf fmt "atomicitySequences: ";
  List.iter summary.atomicitySequences ~f:print_atomicity_sequences;
  F.fprintf fmt "\n"

let convert_astate_to_summary (astate : t) : summary =
  let atomicitySequences : (string list) list ref = ref [] in

  let iterator (astateEl : tElement) : unit =
    let atomicitySequence : string list ref = ref [] in
    let appendToAtomicitySequence : bool ref = ref false in

    let iterator (s : string) : unit =
      if phys_equal (String.compare s "(") 0 then
        appendToAtomicitySequence := true
      else if phys_equal (String.compare s ")") 0 then
      (
        let isSequenceInSequences : bool =
          List.mem !atomicitySequences !atomicitySequence ~equal:lists_equal
        in

        if not isSequenceInSequences then
          atomicitySequences := !atomicitySequences @ [!atomicitySequence];
        appendToAtomicitySequence := false;
        atomicitySequence := []
      )
      else if !appendToAtomicitySequence then
        atomicitySequence := !atomicitySequence @ [s]
    in
    List.iter astateEl.finalCalls ~f:iterator
  in
  TSet.iter astate ~f:iterator;

  {atomicitySequences= !atomicitySequences}

(******************************** Operators ***********************************)

let ( <= ) ~lhs:(lhs : t) ~rhs:(rhs : t) : bool =
  if phys_equal lhs rhs then true else TSet.is_subset lhs ~of_:rhs

let join (astate1 : t) (astate2 : t) : t =
  if phys_equal astate1 astate2 then astate1
  else if TSet.is_empty astate1 then astate2
  else if TSet.is_empty astate2 then astate1
  else TSet.union astate1 astate2

let widen ~prev:(prev : t) ~next:(next : t) ~num_iters:(_ : int) : t =
  join prev next
