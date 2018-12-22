open! IStd
module F = Format

(******************************** Functions ***********************************)

val is_lock : string -> bool

val is_unlock : string -> bool

(******************************** Astate **************************************)

type t

val initial : t

val pp : F.formatter -> t -> unit

val update_astate_on_function_call : t -> string -> t

val update_astate_on_lock : t -> t

val update_astate_on_unlock : t -> t

val update_astate_at_the_end_of_function : t -> t

(******************************** Summary *************************************)

type summary

val pp_summary : F.formatter -> summary -> unit

val update_astate_on_function_call_with_summary : t -> summary -> t

val convert_astate_to_summary : t -> summary

(******************************** Operators ***********************************)

val ( <= ) : lhs:t -> rhs:t -> bool

val join : t -> t -> t

val widen : prev:t -> next:t -> num_iters:int -> t
