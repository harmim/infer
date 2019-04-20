(** Detection of atomicity violations implementation. *)

open! IStd
open! AtomicityUtils

module F = Format
module D = AtomicityViolationsDomain (* The abstract domain definition. *)
module Procname = Typ.Procname
module Loc = Location

(** The summary payload for analyzed functions. *)
module Payload = SummaryPayload.Make (struct
  type t = D.summary (* A type of the payload is the domain summary. *)

  let update_payloads (payload : t) (payloads : Payloads.t) : Payloads.t =
    {payloads with atomicity_violations= Some payload}

  let of_payloads (payloads : Payloads.t) : t option =
    payloads.atomicity_violations
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
      (loc : Loc.t)
    ) ->
      let calleePnameString : string = Procname.to_string calleePname in

      (* let astate : D.t = *)
      if is_lock calleePnameString then D.update_astate_on_lock astate
      else if is_unlock calleePnameString then D.update_astate_on_unlock astate
      else
        let astate : D.t =
          D.update_astate_on_function_call astate calleePnameString loc
        in

        ( match Payload.read procData.pdesc calleePname with
          | Some (summary : D.summary) ->
            D.update_astate_on_function_call_with_summary astate summary loc

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
    F.pp_print_string fmt "AtomicityViolations"
end

(** The analyzer definition. *)
module Analyzer =
  LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

let analyze_procedure (procArgs : Callbacks.proc_callback_args) : Summary.t =
  D.initialise true;

  let procNameString : string =
    Procname.to_string (Procdesc.get_proc_name procArgs.proc_desc)
  and procData : ProcData.no_extras ProcData.t =
    ProcData.make_default procArgs.proc_desc procArgs.tenv
  in

  (* Compute the abstract state for the given function. *)
  match Analyzer.compute_post procData ~initial:D.initial
  with
  | Some (post : D.t) ->
    let convertedSummary : D.summary = D.convert_astate_to_summary post in

    (* A debug log. *)
    let fmt : F.formatter = F.str_formatter
    and _ : string = F.flush_str_formatter () in

    F.fprintf
      fmt
      "\n\nFunction: %s\n%a%a\n\n"
      procNameString D.pp post D.pp_summary convertedSummary;
    Logging.(debug Capture Verbose) "%s" (F.flush_str_formatter ());

    D.report_atomicity_violations
      post ( fun (loc : Loc.t) (msg : string) : unit ->
        Reporting.log_error
          procArgs.summary ~loc:loc IssueType.atomicity_violation msg );

    Payload.update_summary convertedSummary procArgs.summary

  | None ->
    Logging.(die InternalError)
      "The detection of atomicity violations failed to compute a post for '%s'."
      procNameString
