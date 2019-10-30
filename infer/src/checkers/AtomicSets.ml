(** Detection of atomic sets implementation. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd
open! AtomicityUtils

module D = AtomicSetsDomain (* Abstract domain definition. *)
module F = Format
module L = List
module Loc = Location
module OC = Out_channel
module Pdata = ProcData
module Pname = Typ.Procname

(** Summary payload for analysed functions. *)
module Payload = SummaryPayload.Make (struct
  type t = D.summary (* Type of the payload is a domain summary. *)

  let field : (Payloads.t, t option) Field.t = Payloads.Fields.atomic_sets
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
        (_ : Loc.t)
      ) ->
      let calleePnameS : string = Pname.to_string calleePname in

      (* let astate : D.t = *)
      if is_lock calleePnameS then D.update_astate_on_lock astate
      else if is_unlock calleePnameS then D.update_astate_on_unlock astate
      else
        let astate : D.t =
          D.update_astate_on_function_call astate calleePnameS
        in

        (* Update the abstract state with the function summary as well if it is
           possible. *)
        ( match Payload.read
            ~caller_summary:pData.summary ~callee_pname:calleePname
          with
          | Some (summary : D.summary) ->
            D.update_astate_on_function_call_with_summary astate summary

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
    F.pp_print_string fmt "AtomicSets"
end

(** Analyser definition. *)
module Analyser =
  LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

let analyse_procedure (args : Callbacks.proc_callback_args) : Summary.t =
  let pName : Pname.t = Summary.get_proc_name args.summary in
  let pNameS : string = Pname.to_string pName
  and tenv : Tenv.t = Exe_env.get_tenv args.exe_env pName in

  (* Compute the abstract state for a given function. *)
  match Analyser.compute_post
    (Pdata.make_default args.summary tenv) ~initial:D.initial
  with
  | Some (post : D.t) ->
    (* Update the abstract state at the end of a function and convert
       the abstract state to the function summary. *)
    let updatedPost : D.t = D.update_astate_at_the_end_of_function post in
    let convertedSummary : D.summary =
      D.convert_astate_to_summary updatedPost
    in

    (* Debug log. *)
    let fmt : F.formatter = F.str_formatter
    and _ : string = F.flush_str_formatter () in
    F.fprintf
      fmt
      "\n\nFunction: %s\n%a%a\n\n"
      pNameS D.pp updatedPost D.pp_summary convertedSummary;
    Logging.(debug Capture Verbose) "%s" (F.flush_str_formatter ());

    Payload.update_summary convertedSummary args.summary

  | None ->
    Logging.(die InternalError)
      "Detection of atomic sets failed to compute a post for '%s'." pNameS

let print_atomic_sets (args : Callbacks.cluster_callback_args) : unit =
  (* Create a directory for printing. *)
  Utils.create_dir inferDir;

  (* Print to a file. *)
  let oc : OC.t = OC.create ~binary:false atomicSetsFile in
  let print_atomic_sets ((_ : Tenv.t), (summary : Summary.t)) : unit =
    let pName : Pname.t = Summary.get_proc_name summary in

    Option.iter
      (Payload.read ~caller_summary:summary ~callee_pname:pName)
        ~f:( fun (summary : D.summary) : unit ->
        D.print_atomic_sets oc (Pname.to_string pName) summary )
  in
  L.iter args.procedures ~f:print_atomic_sets;
  OC.close oc;

  F.fprintf
    F.std_formatter
    "Detection of atomic sets produced an output into file '%s'.\n"
    atomicSetsFile
