open! IStd
module F = Format
module L = Logging
module Domain = AtomicityDomain

module Payload = SummaryPayload.Make (struct
  type t = Domain.summary

  let update_payloads (payload : t) (payloads : Payloads.t) : Payloads.t =
    {payloads with atomicity= Some payload}

  let of_payloads (payloads : Payloads.t) : t option =
    payloads.atomicity
end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type extras = ProcData.no_extras

  let exec_instr
    (astate : Domain.t)
    (_ : extras ProcData.t)
    (_ : CFG.Node.t)
    (instr : HilInstr.t)
    : Domain.t =
    match instr with
    | Call (
      (_ : AccessPath.base),
      (Direct (calleePname : Typ.Procname.t) : HilInstr.call),
      (_ : HilExp.t list),
      (_ : CallFlags.t),
      (_ : Location.t)
    ) ->
      let calleePnameString : string = Typ.Procname.to_string calleePname in

      if Domain.is_lock calleePnameString then
        Domain.update_astate_on_lock astate
      else if Domain.is_unlock calleePnameString then
        Domain.update_astate_on_unlock astate
      else
        Domain.update_astate_on_function_call astate calleePnameString

    | _ ->
      astate

  let pp_session_name (_ : CFG.Node.t) (fmt : F.formatter) : unit =
    F.fprintf fmt "Atomicity"
end

module Analyzer =
  LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

let checker (procArgs : Callbacks.proc_callback_args) : Summary.t =
  let procNameString : string =
    Typ.Procname.to_string (Procdesc.get_proc_name procArgs.proc_desc)
  in

  F.printf
    "::::::::::::::::::::::::: function %s START :::::::::::::::::::::::::\n"
    procNameString;

  match Analyzer.compute_post
    (ProcData.make_default procArgs.proc_desc procArgs.tenv)
    ~initial:Domain.initial
  with
  | Some (post : Domain.t) ->
    let updatedPost : Domain.t =
      Domain.update_astate_at_the_end_of_function post
    in
    let convertedSummary : Domain.summary =
      Domain.convert_astate_to_summary updatedPost
    in

    Domain.pp F.std_formatter updatedPost;
    Domain.pp_summary F.std_formatter convertedSummary;
    F.printf
      "::::::::::::::::::::::::: function %s END :::::::::::::::::::::::::\n\n"
      procNameString;

    Payload.update_summary convertedSummary procArgs.summary

  | None ->
    L.die
      L.InternalError
      "Atomicity analysis failed to compute post for %s."
      procNameString

let reporting (_ : Callbacks.cluster_callback_args) : unit =
  F.printf ""
