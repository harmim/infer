(** Atomicity detection implementation. *)

open! IStd
module F = Format
module D = AtomicityDetectionDomain (* Domain definition. *)

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
      (Direct (calleePname : Typ.Procname.t) : HilInstr.call),
      (_ : HilExp.t list),
      (_ : CallFlags.t),
      (_ : Location.t)
    ) ->
      let calleePnameString : string = Typ.Procname.to_string calleePname in

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
    Typ.Procname.to_string (Procdesc.get_proc_name procArgs.proc_desc)
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

    F.fprintf F.std_formatter "Function: %s\n" procNameString;
    D.pp F.std_formatter updatedPost;
    D.pp_summary F.std_formatter convertedSummary;
    F.fprintf F.std_formatter "\n\n";

    Payload.update_summary convertedSummary procArgs.summary
  | None ->
    Logging.die
      Logging.InternalError
      "Atomicity detection failed to compute post for %s."
      procNameString

let reporting (_ : Callbacks.cluster_callback_args) : unit = ()
