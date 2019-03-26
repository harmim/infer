(** Detection of atomicity violations implementation. *)

open! IStd
open AtomicityUtils

module D = AtomicityViolationsDomain (* The abstract domain definition. *)

(** The summary payload for analyzed functions. *)
module Payload = SummaryPayload.Make (struct
  type t = D.summary (* A type of the payload is the domain summary. *)

  let update_payloads (payload : t) (payloads : Payloads.t) : Payloads.t =
    {payloads with atomicity_violations= Some payload}

  let of_payloads (payloads : Payloads.t) : t option =
    payloads.atomicity_violations
end)

let analyze_procedure (procArgs : Callbacks.proc_callback_args) : Summary.t =
  Payload.update_summary D.initialSummary procArgs.summary
