(** Detection of atomicity violations domain implementation. *)

open! IStd
open AtomicityUtils

module F = Format

(* ****************************** Astate ************************************ *)

type t = int

let initial : t = 42

let pp (fmt : F.formatter) (astate : t) : unit =
  ()

(* ****************************** Summary *********************************** *)

type summary = t

let initialSummary : summary = 42

let pp_summary (fmt : F.formatter) (summary : summary) : unit =
  pp fmt summary

(* ****************************** Operators ********************************* *)

let ( <= ) ~lhs:(l : t) ~rhs:(r : t) : bool =
  true

let join (astate1 : t) (astate2 : t) : t =
  astate1

let widen ~prev:(p : t) ~next:(n : t) ~num_iters:(i : int) : t =
  p
