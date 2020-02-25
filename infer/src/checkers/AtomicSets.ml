(** Detection of atomic sets implementation. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd
open! AtomicityUtils

module AccessExp = HilExp.AccessExpression
module D = AtomicSetsDomain (* The abstract domain definition. *)
module F = Format
module L = List
module Loc = Location
module OC = Out_channel
module Pdata = ProcData
module Pname = Typ.Procname

(** A summary payload for analysed functions. *)
module Payload = SummaryPayload.Make (struct
  type t = D.summary (* A type of the payload is a domain summary. *)

  let field : (Payloads.t, t option) Field.t = Payloads.Fields.atomic_sets
end)

(** A transfer function for abstract states of an analysed function. *)
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
        (actuals : HilExp.t list),
        (_ : CallFlags.t),
        (_ : Loc.t)
      ) ->
      if f_is_ignored calleePname then astate
      else
      (
        let lockPath : AccessPath.t =
          match L.hd actuals with
          | Some (exp : HilExp.t) ->
            ( match L.hd (HilExp.get_access_exprs exp) with
             | Some (accessExp : AccessExp.t) ->
               AccessExp.to_access_path accessExp

             | None ->
               Logging.(die InternalError)
                "Failed to find an access path of the lock '%a' called from \
                 '%a'."
                HilExp.pp exp Pname.pp calleePname
            )

          | None -> (Var.of_id (Ident.create_none ()), Typ.void), []
        in

        (* let astate : D.t = *)
        if f_is_lock calleePname then D.update_astate_on_lock astate lockPath
        else if f_is_unlock calleePname then
          D.update_astate_on_unlock astate lockPath
        else
          let astate : D.t =
            D.update_astate_on_function_call
              astate (Pname.to_string calleePname)
          in

          (* Update the abstract state with the function summary as well if it
             is possible. *)
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
          "\n\nFunction: %a\n%a\n\n"
          Pname.pp calleePname D.pp astate; *)

        (* astate *)
      )

    | _ -> astate

  let pp_session_name (_ : CFG.Node.t) (fmt : F.formatter) : unit =
    F.pp_print_string fmt "AtomicSets"
end

(** An analyser definition. *)
module Analyser =
  LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

let analyse_procedure (args : Callbacks.proc_callback_args) : Summary.t =
  let pName : Pname.t = Summary.get_proc_name args.summary in

  if f_is_ignored pName then args.summary
  else
  (
    let procData : Pdata.no_extras Pdata.t =
      Pdata.make_default args.summary (Exe_env.get_tenv args.exe_env pName)
    in

    (* Compute the abstract state for a given function. *)
    match Analyser.compute_post procData ~initial:D.initial with
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
        "\n\nFunction: %a\n%a%a\n\n"
        Pname.pp pName D.pp updatedPost D.pp_summary convertedSummary;
      Logging.(debug Capture Verbose) "%s" (F.flush_str_formatter ());

      Payload.update_summary convertedSummary args.summary

    | None ->
      Logging.(die InternalError)
        "The detection of atomic sets failed to compute a post for '%a'."
        Pname.pp pName
  )

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
    "The detection of atomic sets produced an output into the file '%s'.\n"
    atomicSetsFile
