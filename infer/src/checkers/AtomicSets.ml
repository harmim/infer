(* Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd
module Domain = AtomicSetsDomain
module F = Format
module L = Logging

(** Detection of atomic sets implementation. *)

(** A summary payload for analysed functions. *)
module Payload = SummaryPayload.Make (struct
  type t = Domain.Summary.t (* A type of the payload is a domain summary. *)

  let field : (Payloads.t, t option) Field.t = Payloads.Fields.atomic_sets
end)

(** A transfer function for abstract states of an analysed function. *)
module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type extras = ProcData.no_extras (* No extras needed. *)

  let exec_instr (astate : Domain.t) (pData : extras ProcData.t) (_ : CFG.Node.t)
      (instr : HilInstr.t) : Domain.t =
    match instr with
    | Call
        ( (_ : AccessPath.base)
        , (Direct (calleePname : Procname.t) : HilInstr.call)
        , (actuals : HilExp.t list)
        , (_ : CallFlags.t)
        , (_ : Location.t) )
      when AtomicityUtils.f_is_ignored calleePname ~actuals:(Some actuals) ->
        astate
    (* Update the abstract state on function calls. *)
    | Call
        ( (_ : AccessPath.base)
        , (Direct (calleePname : Procname.t) : HilInstr.call)
        , (actuals : HilExp.t list)
        , (_ : CallFlags.t)
        , (_ : Location.t) ) -> (
        let update_astate_with_locks (astate : Domain.t) (locks : HilExp.t list)
            ~(f : ?ap:AccessPath.t option -> Domain.t -> Domain.t) : Domain.t =
          let astate : Domain.t ref = ref astate in
          List.iter locks ~f:(fun (lock : HilExp.t) ->
              astate := f ~ap:(AtomicityUtils.get_lock_path lock) !astate) ;
          !astate
        in
        match ConcurrencyModels.get_lock_effect calleePname actuals with
        (* lock *)
        | Lock (locks : HilExp.t list) ->
            update_astate_with_locks astate locks ~f:Domain.apply_lock
        | GuardConstruct {guard; acquire_now= true} | GuardLock (guard : HilExp.t) ->
            update_astate_with_locks astate [guard] ~f:Domain.apply_lock
        (* unlock *)
        | Unlock (locks : HilExp.t list) ->
            update_astate_with_locks astate locks ~f:Domain.apply_unlock
        | GuardUnlock (guard : HilExp.t) | GuardDestroy (guard : HilExp.t) ->
            update_astate_with_locks astate [guard] ~f:Domain.apply_unlock
        (* TODO: try lock *)
        | LockedIfTrue (_ : HilExp.t list) ->
            astate
        | GuardLockedIfTrue (_ : HilExp.t) ->
            astate
        (* function call *)
        | NoEffect -> (
            let astate : Domain.t = Domain.apply_call astate (Procname.to_string calleePname) in
            (* Update the abstract state with the function summary as well if it is possible. *)
            match Payload.read ~caller_summary:pData.summary ~callee_pname:calleePname with
            | Some (summary : Domain.Summary.t) ->
                Domain.apply_summary astate summary
            | None ->
                astate )
        | _ ->
            astate )
    | _ ->
        astate


  let pp_session_name (_ : CFG.Node.t) (fmt : F.formatter) : unit =
    F.pp_print_string fmt "AtomicSets"
end

(** An analyser definition. *)
module Analyser = LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

let analyse_procedure (args : Callbacks.proc_callback_args) : Summary.t =
  let pName : Procname.t = Summary.get_proc_name args.summary in
  if AtomicityUtils.f_is_ignored pName then args.summary
  else
    let procData : ProcData.no_extras ProcData.t =
      ProcData.make_default args.summary (Exe_env.get_tenv args.exe_env pName)
    and pre : Domain.t =
      if Procdesc.is_java_synchronized args.summary.proc_desc then Domain.apply_lock Domain.initial
      else Domain.initial
    in
    (* Compute the abstract state for a given function. *)
    match Analyser.compute_post procData ~initial:pre with
    | Some (post : Domain.t) ->
        (* Update the abstract state at the end of a function and convert the abstract state to the
           function summary. *)
        let updatedPost : Domain.t = Domain.update_at_the_end_of_function post in
        let summary : Domain.Summary.t = Domain.Summary.make updatedPost in
        (* Debug log. *)
        let fmt : F.formatter = F.str_formatter and (_ : string) = F.flush_str_formatter () in
        F.fprintf fmt "\n\nFunction: %a\n%a%a\n\n" Procname.pp pName Domain.pp updatedPost
          Domain.Summary.pp summary ;
        L.(debug Capture Verbose) "%s" (F.flush_str_formatter ()) ;
        Payload.update_summary summary args.summary
    | None ->
        L.(die InternalError)
          "The detection of atomic sets failed to compute a post for '%a'." Procname.pp pName


let print_atomic_sets (args : Callbacks.file_callback_args) : IssueLog.t =
  (* Create a directory for printing. *)
  Utils.create_dir AtomicityUtils.inferDir ;
  (* Print to a file. *)
  let oc : Out_channel.t =
    Out_channel.create ~binary:false ~append:Config.atomic_sets_file_append ~fail_if_exists:false
      AtomicityUtils.atomicSetsFile
  in
  let print_atomic_sets (pName : Procname.t) : unit =
    Option.iter (Payload.read_toplevel_procedure pName) ~f:(fun (summary : Domain.Summary.t) ->
        Domain.Summary.print_atomic_sets summary ~f_name:(Procname.to_string pName) oc)
  in
  List.iter args.procedures ~f:print_atomic_sets ;
  Out_channel.close oc ;
  F.fprintf F.std_formatter "The detection of atomic sets produced an output into the file '%s'.\n"
    AtomicityUtils.atomicSetsFile ;
  IssueLog.empty
