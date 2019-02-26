(** Atomicity detection implementation. *)

open! IStd
module F = Format
module D = AtomicityDetectionDomain (* The abstract domain definition. *)
module Procname = Typ.Procname
module OC = Out_channel

(** The summary payload for analyzed functions. *)
module Payload = SummaryPayload.Make (struct
  type t = D.summary (* A type of the payload is the domain summary. *)

  let update_payloads (payload : t) (payloads : Payloads.t) : Payloads.t =
    {payloads with atomicityDetection= Some payload}

  let of_payloads (payloads : Payloads.t) : t option =
    payloads.atomicityDetection
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
      (_ : Location.t)
    ) ->
      let calleePnameString : string = Procname.to_string calleePname in

      (* let astate : D.t = *)
      if D.is_lock calleePnameString then D.update_astate_on_lock astate
      else if D.is_unlock calleePnameString then
        D.update_astate_on_unlock astate
      else
      (
        let astate : D.t =
          D.update_astate_on_function_call astate calleePnameString
        in

        (* Update the abstract state with the function summary as well, if it is
           possible. *)
        match Payload.read procData.pdesc calleePname with
        | Some (summary : D.summary) ->
          D.update_astate_on_function_call_with_summary astate summary
        | None -> astate
      )
      (* in *)

      (* F.fprintf F.std_formatter "\nFunction: %s\n" calleePnameString; *)
      (* D.pp F.std_formatter astate; *)
      (* F.fprintf F.std_formatter "\n\n"; *)

      (* astate *)
    | _ -> astate

  let pp_session_name (_ : CFG.Node.t) (fmt : F.formatter) : unit =
    F.pp_print_string fmt "Atomicity detection"
end

(** The analyzer definition. *)
module Analyzer =
  LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

let checker (procArgs : Callbacks.proc_callback_args) : Summary.t =
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
    F.fprintf fmt "\n\nFunction: %s\n" procNameString;
    D.pp fmt updatedPost;
    D.pp_summary fmt convertedSummary;
    F.pp_print_string fmt "\n";
    Logging.(debug Capture Verbose) "%s" (F.flush_str_formatter ());

    Payload.update_summary convertedSummary procArgs.summary
  | None ->
    Logging.(die InternalError)
      "The atomicity detection failed to compute a post for the %s."
      procNameString

let reporting (clusterArgs : Callbacks.cluster_callback_args) : unit =
  (* Create a directory for the reporting. *)
  let dir : string =
    Escape.escape_filename (CommandLineOption.init_work_dir ^ "/infer-out")
  in
  Utils.create_dir dir;

  (* Report to the file. *)
  let oc : OC.t = OC.create ~binary:false (dir ^ "/atomicity-detection") in
  let report ((_ : Tenv.t), (procDesc : Procdesc.t)) : unit =
    let procName : Procname.t = Procdesc.get_proc_name procDesc in
    Option.iter
      (Payload.read procDesc procName) ~f:(fun (summary : D.summary) : unit ->
        D.report oc (Procname.to_string procName) summary)
  in
  List.iter clusterArgs.procedures ~f:report;
  OC.close oc;

  F.pp_print_string
    F.std_formatter
    ("The atomicity detection produced an output (atomicity sequences) into " ^
     "the './infer-out/atomicity-detection' file.\n")
