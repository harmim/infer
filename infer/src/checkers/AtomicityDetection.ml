(** Atomicity detection implementation. *)

open! IStd
module F = Format
module D = AtomicityDetectionDomain (* Domain definition. *)
module Procname = Typ.Procname
module OC = Out_channel

(** Summary payload for analyzed functions. *)
module Payload = SummaryPayload.Make (struct
  type t = D.summary (* Type of payload is domain summary. *)

  let update_payloads (payload : t) (payloads : Payloads.t) : Payloads.t =
    {payloads with atomicityDetection= Some payload}

  let of_payloads (payloads : Payloads.t) : t option =
    payloads.atomicityDetection
end)

(** Transfer function for abstract states of analyzed function. *)
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
    (* Update abstract state on functions calls. *)
    | Call (
      (_ : AccessPath.base),
      (Direct (calleePname : Procname.t) : HilInstr.call),
      (_ : HilExp.t list),
      (_ : CallFlags.t),
      (_ : Location.t)
    ) ->
      let calleePnameString : string = Procname.to_string calleePname in

      if D.is_lock calleePnameString then D.update_astate_on_lock astate
      else if D.is_unlock calleePnameString then
        D.update_astate_on_unlock astate
      else
      (
        let astate : D.t =
          D.update_astate_on_function_call astate calleePnameString
        in

        (* Update abstract state with function summary as well, if it is
           possible. *)
        match Payload.read procData.pdesc calleePname with
        | Some (summary : D.summary) ->
          D.update_astate_on_function_call_with_summary astate summary
        | None -> astate
      )
    | _ -> astate

  let pp_session_name (_ : CFG.Node.t) (fmt : F.formatter) : unit =
    F.pp_print_string fmt "Atomicity detection"
end

(** Analyzer definition. *)
module Analyzer =
  LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

let checker (procArgs : Callbacks.proc_callback_args) : Summary.t =
  let procNameString : string =
    Procname.to_string (Procdesc.get_proc_name procArgs.proc_desc)
  in

  (* Compute abstract state for given function. *)
  match Analyzer.compute_post
    (ProcData.make_default procArgs.proc_desc procArgs.tenv) ~initial:D.initial
  with
  | Some (post : D.t) ->
    (* Update abstract state at the end of function and convert
       abstract state to function summary. *)
    let updatedPost : D.t = D.update_astate_at_the_end_of_function post in
    let convertedSummary : D.summary =
      D.convert_astate_to_summary updatedPost
    in

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
      "Atomicity detection failed to compute post for %s." procNameString

let reporting (clusterArgs : Callbacks.cluster_callback_args) : unit =
  let dir : string =
    Escape.escape_filename (CommandLineOption.init_work_dir ^ "/infer-out")
  in
  Utils.create_dir dir;

  let oc : OC.t = OC.create ~binary:false (dir ^ "/atomicity-detection") in
  let iterator ((_ : Tenv.t), (procDesc : Procdesc.t)) : unit =
    let procName : Procname.t = Procdesc.get_proc_name procDesc in
    let iterator (summary : D.summary) : unit =
      D.report oc (Procname.to_string procName) summary
    in
    Option.iter (Payload.read procDesc procName) ~f:iterator
  in
  List.iter clusterArgs.procedures ~f:iterator;
  OC.close oc;

  F.pp_print_string
    F.std_formatter
    ("Atomicity detection produced output (atomicity sequences) into " ^
     "'./infer-out/atomicity-detection' file.\n")
