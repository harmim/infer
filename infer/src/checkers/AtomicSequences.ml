(** Detection of atomic sequences implementation. *)

open! IStd
open! AtomicityUtils

module F = Format
module D = AtomicSequencesDomain (* The abstract domain definition. *)
module Procname = Typ.Procname
module OC = Out_channel
module L = List
module Loc = Location

(** The summary payload for analyzed functions. *)
module Payload = SummaryPayload.Make (struct
  type t = D.summary (* A type of the payload is the domain summary. *)

  let update_payloads (payload : t) (payloads : Payloads.t) : Payloads.t =
    {payloads with atomic_sequences= Some payload}

  let of_payloads (payloads : Payloads.t) : t option = payloads.atomic_sequences
end)

(** The transfer function for abstract states of the analyzed function. *)
module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = D

  type extras = ProcData.no_extras (* No extras needed. *)

  let exec_instr
    (astate : D.t)
    (procData : extras ProcData.t)
    (_ : CFG.Node.t)
    (instr : HilInstr.t)
    : D.t =
    match instr with
    (* Update the abstract state on functions calls. *)
    | Call (
      (_ : AccessPath.base),
      (Direct (calleePname : Procname.t) : HilInstr.call),
      (_ : HilExp.t list),
      (_ : CallFlags.t),
      (_ : Loc.t)
    ) ->
      let calleePnameString : string = Procname.to_string calleePname in

      (* let astate : D.t = *)
      if is_lock calleePnameString then D.update_astate_on_lock astate
      else if is_unlock calleePnameString then D.update_astate_on_unlock astate
      else
        let astate : D.t =
          D.update_astate_on_function_call astate calleePnameString
        in

        (* Update the abstract state with the function summary as well, if it is
           possible. *)
        ( match Payload.read procData.pdesc calleePname with
          | Some (summary : D.summary) ->
            D.update_astate_on_function_call_with_summary astate summary

          | None -> astate
        )
      (* in *)

      (* F.fprintf
        F.std_formatter
        "\n\nFunction: %s\n%a\n\n"
        calleePnameString D.pp astate; *)

      (* astate *)

    | _ -> astate

  let pp_session_name (_ : CFG.Node.t) (fmt : F.formatter) : unit =
    F.pp_print_string fmt "AtomicSequences"
end

(** The analyzer definition. *)
module Analyzer =
  LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

let analyze_procedure (procArgs : Callbacks.proc_callback_args) : Summary.t =
  let procNameString : string =
    Procname.to_string (Procdesc.get_proc_name procArgs.proc_desc)
  in

  (* Compute the abstract state for the given function. *)
  match Analyzer.compute_post
    (ProcData.make_default procArgs.proc_desc procArgs.tenv) ~initial:D.initial
  with
  | Some (post : D.t) ->
    (* Update the abstract state at the end of the function and convert
       the abstract state to the function summary. *)
    let updatedPost : D.t = D.update_astate_at_the_end_of_function post in
    let convertedSummary : D.summary =
      D.convert_astate_to_summary updatedPost
    in

    (* A debug log. *)
    let fmt : F.formatter = F.str_formatter
    and _ : string = F.flush_str_formatter () in

    F.fprintf
      fmt
      "\n\nFunction: %s\n%a%a\n\n"
      procNameString D.pp updatedPost D.pp_summary convertedSummary;
    Logging.(debug Capture Verbose) "%s" (F.flush_str_formatter ());

    Payload.update_summary convertedSummary procArgs.summary

  | None ->
    Logging.(die InternalError)
      "The detection of atomic sequences failed to compute a post for '%s'."
      procNameString

let print_atomic_sequences
  (clusterArgs : Callbacks.cluster_callback_args) : unit =
  (* Create a directory for the printing. *)
  Utils.create_dir inferDir;

  (* Print to the file. *)
  let oc : OC.t = OC.create ~binary:false atomicSequencesFile in
  let print_atomic_sequences ((_ : Tenv.t), (procDesc : Procdesc.t)) : unit =
    let procName : Procname.t = Procdesc.get_proc_name procDesc in

    Option.iter
      (Payload.read procDesc procName) ~f:( fun (summary : D.summary) : unit ->
        D.print_atomic_sequences oc (Procname.to_string procName) summary )
  in
  L.iter clusterArgs.procedures ~f:print_atomic_sequences;
  OC.close oc;

  F.fprintf
    F.std_formatter
    "The detection of atomic sequences produced an output (atomic sequences) into file '%s'.\n"
    atomicSequencesFile
