exception Empty

(** Page 8 - Signature for stacks. *)
module type Stack =
sig
  type 'a stack

  val empty   : 'a stack
  val isEmpty : 'a stack -> bool

  val cons    : 'a -> 'a stack -> 'a stack
  val head    : 'a stack -> 'a
  val tail    : 'a stack -> 'a stack
end

(** Page 12 - Signature for sets. *)
module type Set =
sig
  type elem
  type set

  val empty  : set
  val insert : elem -> set -> set
  val member : elem -> set -> bool
end

(** Page 14 - Implementation of binary search trees as a functor. *)
module type Ordered =
(* a totally ordered type and its comparison functions *)
sig
  type t

  val eq  : t -> t -> bool
  val lt  : t -> t -> bool
  val leq : t -> t -> bool
end

(** Page 18 - Signature for heaps (priority queues). *)
module type Heap =
sig
  module Elem : Ordered

  type heap

  val empty     : heap
  val isEmpty   : heap -> bool

  val insert    : Elem.t -> heap -> heap
  val merge     : heap -> heap -> heap

  val findMin   : heap -> Elem.t (* raises Empty if heap is empty *)
  val deleteMin : heap -> heap   (* raises Empty if heap is empty *)
end
