(** Page 14 - Implementation of binary search trees as a functor. *)
module type Ordered =
(* a totally ordered type and its comparison functions *)
sig
  type t

  val eq  : t * t -> bool
  val lt  : t * t -> bool
  val leq : t * t -> bool
end
