(** Detection of atomic sets implementation. *)
(** Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd
open! AtomicityUtils

module D = AtomicSetsDomain (* The abstract domain definition. *)
module F = Format
module L = List
module Opt = Option

(** A summary payload for analysed functions. *)
module Payload = SummaryPayload.Make (struct
  type t = D.summary (* A type of the payload is a domain summary. *)

  let field : (Payloads.t, t option) Field.t = Payloads.Fields.atomic_sets
end)

(** A transfer function for abstract states of an analysed function. *)
module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = D

  type extras = ProcData.no_extras (* No extras needed. *)

  let exec_instr
    (astate : D.t)
    (pData : extras ProcData.t)
    (_ : CFG.Node.t)
    (instr : HilInstr.t)
    : D.t =
    match instr with
    Call (
      (_ : AccessPath.base),
      (Direct (calleePname : Procname.t) : HilInstr.call),
      (actuals : HilExp.t list),
      (_ : CallFlags.t),
      (_ : Location.t)
    ) when f_is_ignored calleePname ~actualsOpt:(Some actuals) -> astate

    (* Update the abstract state on function calls. *)
    | Call (
        (_ : AccessPath.base),
        (Direct (calleePname : Procname.t) : HilInstr.call),
        (actuals : HilExp.t list),
        (_ : CallFlags.t),
        (_ : Location.t)
      ) ->
      let update_astate_with_locks
        (astate : D.t)
        (f : (D.t -> AccessPath.t option -> D.t))
        (locks : HilExp.t list)
        : D.t =
        let astate : D.t ref = ref astate in

        L.iter locks ~f:( fun (lock : HilExp.t) : unit ->
          astate := f !astate (get_lock_path lock) );

        !astate
      in

      (* let astate : D.t = *)
      ( match ConcurrencyModels.get_lock_effect calleePname actuals with
        (* lock *)
        Lock (locks : HilExp.t list) ->
          update_astate_with_locks astate D.update_astate_on_lock locks
        | GuardConstruct {guard= guard; acquire_now= true}
        | GuardLock (guard : HilExp.t) ->
          update_astate_with_locks astate D.update_astate_on_lock [guard]

        (* unlock *)
        | Unlock (locks : HilExp.t list) ->
          update_astate_with_locks astate D.update_astate_on_unlock locks
        | GuardUnlock (guard : HilExp.t)
        | GuardDestroy (guard : HilExp.t) ->
          update_astate_with_locks astate D.update_astate_on_unlock [guard]

        (* TODO: try lock *)
        | LockedIfTrue (_ : HilExp.t list) -> astate
        | GuardLockedIfTrue (_ : HilExp.t) -> astate

        (* function call *)
        | NoEffect ->
          let astate : D.t =
            D.update_astate_on_function_call
              astate (Procname.to_string calleePname)
          in

          (* Update the abstract state with the function summary as well if it
             is possible. *)
          ( match Payload.read
              ~caller_summary:pData.summary ~callee_pname:calleePname
            with
            Some (summary : D.summary) ->
              D.update_astate_on_function_call_with_summary astate summary

            | None -> astate
          )

        | _ -> astate
      )
      (* in *)

      (* F.fprintf
        F.std_formatter
        "\n\nFunction: %a\n%a\n\n"
        Procname.pp calleePname D.pp astate; *)

      (* astate *)

    | _ -> astate

  let pp_session_name (_ : CFG.Node.t) (fmt : F.formatter) : unit =
    F.pp_print_string fmt "AtomicSets"
end

(** An analyser definition. *)
module Analyser =
  LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

let analyse_procedure (args : Callbacks.proc_callback_args) : Summary.t =
  let pName : Procname.t = Summary.get_proc_name args.summary in

  if f_is_ignored pName then args.summary
  else (
    let procData : ProcData.no_extras ProcData.t =
      ProcData.make_default args.summary (Exe_env.get_tenv args.exe_env pName)
    and initialPost : D.t =
      if Procdesc.is_java_synchronized args.summary.proc_desc then
        D.update_astate_on_lock D.initial None
      else D.initial
    in

    (* Compute the abstract state for a given function. *)
    match Analyser.compute_post procData ~initial:initialPost with
    Some (post : D.t) ->
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
        Procname.pp pName D.pp updatedPost D.pp_summary convertedSummary;
      Logging.(debug Capture Verbose) "%s" (F.flush_str_formatter ());

      Payload.update_summary convertedSummary args.summary

    | None ->
      Logging.(die InternalError)
        "The detection of atomic sets failed to compute a post for '%a'."
        Procname.pp pName
  )

let print_atomic_sets (args : Callbacks.file_callback_args) : IssueLog.t =
  (* Create a directory for printing. *)
  Utils.create_dir inferDir;

  (* Print to a file. *)
  let oc : Out_channel.t =
    Out_channel.create
      ~binary:false
      ~append:Config.atomic_sets_file_append
      ~fail_if_exists:false
      atomicSetsFile
  in
  let print_atomic_sets (pName : Procname.t) : unit =
    Opt.iter
      (Payload.read_toplevel_procedure pName)
      ~f:( fun (summary : D.summary) : unit ->
        D.print_atomic_sets oc (Procname.to_string pName) summary )
  in
  L.iter args.procedures ~f:print_atomic_sets;
  Out_channel.close oc;

  F.fprintf
    F.std_formatter
    "The detection of atomic sets produced an output into the file '%s'.\n"
    atomicSetsFile;

  IssueLog.empty
