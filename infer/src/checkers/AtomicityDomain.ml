open! IStd
module F = Format

(******************************** Functions ***********************************)

let are_strings_equal (s1 : string) (s2 : string) : bool =
  phys_equal (String.compare s1 s2) 0

let print_string_with_space (fmt : F.formatter) (s : string) : unit =
  F.fprintf fmt "%s " s

let is_lock (functionName : string) : bool =
  phys_equal (String.compare functionName "pthread_mutex_lock") 0

let is_unlock (functionName : string) : bool =
  phys_equal (String.compare functionName "pthread_mutex_unlock") 0

(******************************** Astate **************************************)

type t =
  { firstOccurrences : string list
  ; callSequence : string list
  ; finalCalls : string list }

let initial : t =
  { firstOccurrences : string list= []
  ; callSequence : string list= []
  ; finalCalls : string list= [] }

let first_occurrences_add_function
  (firstOccurrences : string list) (functionName : string) : string list =
  if List.mem firstOccurrences functionName ~equal:are_strings_equal then
    firstOccurrences
  else
    firstOccurrences @ [functionName]

let call_sequence_add_lock (callSequence : string list) : string list =
  callSequence @ ["("]

let call_sequence_add_unlock (callSequence : string list) : string list =
  callSequence @ [")"]

let call_sequence_add_first_occurences
  (callSequence : string list) (firstOccurrences : string list) : string list =
  callSequence @ firstOccurrences

let final_calls_add_call_sequence
  (finalCalls : string list) (callSequence : string list) : string list =
  let is_call_sequence_in_final_calls : bool =
    let finalCallsLength : int = List.length finalCalls in
    let callSequenceLength : int = List.length callSequence in

    if callSequenceLength > finalCallsLength then
      false
    else
    (
      let i : int ref = ref 0 in
      let j : int ref = ref 0 in

      while !i < finalCallsLength && not (phys_equal !j callSequenceLength) do
        let comparedElements : int =
          String.compare
            (List.nth_exn finalCalls !i) (List.nth_exn callSequence !j)
        in
        if phys_equal comparedElements 0 then
          j := !j + 1
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

  if is_call_sequence_in_final_calls then
    finalCalls
  else
    finalCalls @ callSequence

let update_astate_at_the_end_of_function (astate : t) : t =
  let finalCalls : string list =
    if List.is_empty astate.callSequence then
      astate.finalCalls @ astate.firstOccurrences
    else
      let callSequence : string list =
        call_sequence_add_unlock
          (call_sequence_add_first_occurences
            astate.callSequence astate.firstOccurrences)
      in

      final_calls_add_call_sequence astate.finalCalls callSequence
  in

  {firstOccurrences= []; callSequence= []; finalCalls= finalCalls}

let pp (fmt : F.formatter) (astate : t) : unit =
  F.fprintf fmt "finalCalls: ";
  List.iter astate.finalCalls ~f:(print_string_with_space fmt);
  F.fprintf fmt "\n"

(******************************** Summary *************************************)

type summary = {atomicitySequences : (string list) list}

let pp_summary (fmt : F.formatter) (summary: summary) : unit =
  let print_atomicity_sequences (sequence : string list) : unit =
    F.fprintf fmt "( ";
    List.iter sequence ~f:(print_string_with_space fmt);
    F.fprintf fmt ") "
  in

  F.fprintf fmt "atomicitySequences: ";
  List.iter summary.atomicitySequences ~f:print_atomicity_sequences;
  F.fprintf fmt "\n"

let convert_astate_to_summary (astate : t) : summary =
  let convert_final_calls_to_atomicity_sequences : (string list) list =
    let atomicitySequences : (string list) list ref = ref [] in

    let atomicitySequence : string list ref = ref [] in
    let appendToAtomicitySequence : bool ref = ref false in
    let iterator (s : string) : unit =
      if phys_equal (String.compare s "(") 0 then
        appendToAtomicitySequence := true
      else if phys_equal (String.compare s ")") 0 then
      (
        let atomicity_sequences_add_sequence
          (sequences : (string list) list)
          (sequence : string list)
          : (string list) list =
          let are_lists_equal (l1 : string list) (l2 : string list) : bool =
            let mem_all (l1 : string list) (l2 : string list) : bool =
              let memAll : bool ref = ref true in

              let iterator (s : string) : unit =
                if not (List.mem l2 s ~equal:are_strings_equal) then
                  memAll := false
              in
              List.iter l1 ~f:iterator;

              !memAll
            in

            (mem_all l1 l2) && (mem_all l2 l1)
          in

          if List.mem sequences sequence ~equal:are_lists_equal then
            sequences
          else
            sequences @ [sequence]
        in

        appendToAtomicitySequence := false;
        atomicitySequences := atomicity_sequences_add_sequence
          !atomicitySequences !atomicitySequence;
        atomicitySequence := []
      )
      else if !appendToAtomicitySequence then
        atomicitySequence := !atomicitySequence @ [s]
    in
    List.iter astate.finalCalls ~f:iterator;

    !atomicitySequences
  in

  {atomicitySequences= convert_final_calls_to_atomicity_sequences}

(******************************** Operators ***********************************)

let ( <= ) ~(lhs : t) ~(rhs : t) : bool =
  lhs <= rhs

let join (stateA : t) (stateB : t) : t =
  Pervasives.max stateA stateB

let widen ~(prev : t) ~(next : t) ~num_iters:(_ : int) : t =
  join prev next
