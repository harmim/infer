(* Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz> *)

open! IStd
module Domain = AtomicityViolationsDomain
module F = Format
module L = Logging

(** Detection of atomicity violations implementation. *)

(** A transfer function for abstract states of an analysed function. *)
module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = Domain.Summary.t InterproceduralAnalysis.t

  let exec_instr (astate : Domain.t) (analysis_data : analysis_data) (_ : CFG.Node.t)
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
        , (loc : Location.t) ) -> (
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
            let astate : Domain.t = Domain.apply_call astate (Procname.to_string calleePname) loc in
            (* Update the abstract state with the function summary as well if it is possible. *)
            match analysis_data.analyze_dependency calleePname with
            | Some ((_ : Procdesc.t), (summary : Domain.Summary.t)) ->
                Domain.apply_summary astate summary loc
            | None ->
                astate )
        | _ ->
            astate )
    | _ ->
        astate


  let pp_session_name (_ : CFG.Node.t) (fmt : F.formatter) : unit =
    F.pp_print_string fmt "AtomicityViolations"
end

(** An analyser definition. *)
module Analyser = LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

let analyse_procedure (analysis_data : Domain.Summary.t InterproceduralAnalysis.t) :
    Domain.Summary.t option =
  (* Domain initialisation. *)
  Domain.initialise () ;
  let pName : Procname.t = Procdesc.get_proc_name analysis_data.proc_desc in
  if AtomicityUtils.f_is_ignored pName then None
  else
    let pre : Domain.t =
      if Procdesc.is_java_synchronized analysis_data.proc_desc then Domain.apply_lock Domain.initial
      else Domain.initial
    in
    (* Compute the abstract state for a given function. *)
    match Analyser.compute_post analysis_data ~initial:pre analysis_data.proc_desc with
    | Some (post : Domain.t) ->
        (* Convert the abstract state to the function summary. *)
        let summary : Domain.Summary.t = Domain.Summary.make post in
        (* Debug log. *)
        let fmt : F.formatter = F.str_formatter and (_ : string) = F.flush_str_formatter () in
        F.fprintf fmt "\n\nFunction: %a\n%a%a\n\n" Procname.pp pName Domain.pp post
          Domain.Summary.pp summary ;
        L.(debug Capture Verbose) "%s" (F.flush_str_formatter ()) ;
        (* Report atomicity violations. *)
        Domain.report_atomicity_violations post ~f:(fun (loc : Location.t) ~(msg : string) ->
            Reporting.log_issue analysis_data.proc_desc analysis_data.err_log ~loc
              AtomicityViolations IssueType.atomicity_violation msg) ;
        Some summary
    | None ->
        L.(die InternalError)
          "The detection of atomicity violations failed to compute a post for '%a'." Procname.pp
          pName
