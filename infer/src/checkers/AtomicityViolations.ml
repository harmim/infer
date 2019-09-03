(** Detection of atomicity violations implementation. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd
open! AtomicityUtils

module D = AtomicityViolationsDomain (* Abstract domain definition. *)
module F = Format
module Loc = Location
module Pdata = ProcData
module Pdesc = Procdesc
module Pname = Typ.Procname

(** Summary payload for analysed functions. *)
module Payload = SummaryPayload.Make (struct
  type t = D.summary (* Type of the payload is a domain summary. *)

  let update_payloads (payload : t) (payloads : Payloads.t) : Payloads.t =
    {payloads with atomicity_violations= Some payload}

  let of_payloads (payloads : Payloads.t) : t option =
    payloads.atomicity_violations
end)

(** Transfer function for abstract states of an analysed function. *)
module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = D

  type extras = Pdata.no_extras (* No extras needed. *)

  let exec_instr
    (astate : D.t)
    (pData : extras Pdata.t)
    (_ : CFG.Node.t)
    (instr : HilInstr.t)
    : D.t =
    match instr with
    (* Update the abstract state on function calls. *)
    | Call (
        (_ : AccessPath.base),
        (Direct (calleePname : Pname.t) : HilInstr.call),
        (_ : HilExp.t list),
        (_ : CallFlags.t),
        (loc : Loc.t)
      ) ->
      let calleePnameS : string = Pname.to_string calleePname in

      (* let astate : D.t = *)
      if is_lock calleePnameS then D.update_astate_on_lock astate
      else if is_unlock calleePnameS then D.update_astate_on_unlock astate
      else
        let astate : D.t =
          D.update_astate_on_function_call astate calleePnameS loc
        in

        (* Update the abstract state with the function summary as well if it is
           possible. *)
        ( match Payload.read pData.pdesc calleePname with
          | Some (summary : D.summary) ->
            D.update_astate_on_function_call_with_summary astate summary loc

          | None -> astate
        )
      (* in *)

      (* F.fprintf
        F.std_formatter
        "\n\nFunction: %s\n%a\n\n"
        calleePnameS D.pp astate; *)

      (* astate *)

    | _ -> astate

  let pp_session_name (_ : CFG.Node.t) (fmt : F.formatter) : unit =
    F.pp_print_string fmt "AtomicityViolations"
end

(** Analyser definition. *)
module Analyser =
  LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

let analyse_procedure (args : Callbacks.proc_callback_args) : Summary.t =
  D.initialise true; (* Domain initialisation. *)

  let pNameS : string = Pname.to_string (Pdesc.get_proc_name args.proc_desc) in

  (* Compute the abstract state for a given function. *)
  match Analyser.compute_post
    (Pdata.make_default args.proc_desc args.tenv) ~initial:D.initial
  with
  | Some (post : D.t) ->
    (* Convert the abstract state to the function summary. *)
    let convertedSummary : D.summary = D.convert_astate_to_summary post in

    (* Debug log. *)
    let fmt : F.formatter = F.str_formatter
    and _ : string = F.flush_str_formatter () in
    F.fprintf
      fmt
      "\n\nFunction: %s\n%a%a\n\n"
      pNameS D.pp post D.pp_summary convertedSummary;
    Logging.(debug Capture Verbose) "%s" (F.flush_str_formatter ());

    (* Report atomicity violations. *)
    D.report_atomicity_violations
      post ( fun (loc : Loc.t) (msg : string) : unit ->
        Reporting.log_error
          args.summary ~loc:loc IssueType.atomicity_violation msg );

    Payload.update_summary convertedSummary args.summary

  | None ->
    Logging.(die InternalError)
      "Detection of atomicity violations failed to compute a post for '%s'."
      pNameS
