exception Empty

(** Page 8 - Figure 2.1.
    Signature for stacks. *)
module type STACK = sig
  type 'a stack

  val empty : 'a stack

  val isEmpty : 'a stack -> bool

  val cons : 'a -> 'a stack -> 'a stack

  val head : 'a stack -> 'a

  val tail : 'a stack -> 'a stack
end

(** Page 12 - Figure 2.7.
    Signature for sets. *)
module type SET = sig
  type elem

  type set

  val empty : set

  val insert : elem -> set -> set

  val member : elem -> set -> bool
end

(** Page 14 - Figure 2.9.
    Implementation of binary search trees as a functor. *)
module type ORDERED = (* a totally ordered type and its comparison functions *)
sig
  type t

  val eq : t -> t -> bool

  val lt : t -> t -> bool

  val leq : t -> t -> bool
end

(** Page 18 - Figure 3.1.
    Signature for heaps (priority queues). *)
module type HEAP = sig
  module Elem : ORDERED

  type heap

  val empty : heap

  val isEmpty : heap -> bool

  val insert : Elem.t -> heap -> heap

  val merge : heap -> heap -> heap

  val findMin : heap -> Elem.t (* raises Empty if heap is empty *)

  val deleteMin : heap -> heap (* raises Empty if heap is empty *)
end

(** Page 36 - Figure 4.1.
    A small streams package. *)
module type STREAM = sig
  type 'a streamCell = Nil | Cons of 'a * 'a stream

  and 'a stream = 'a streamCell Lazy.t

  val ( ++ ) : 'a stream -> 'a stream -> 'a stream

  val take : int -> 'a stream -> 'a stream

  val drop : int -> 'a stream -> 'a stream

  val reverse : 'a stream -> 'a stream
end

(** Page 42 - Figure 5.1.
    Signature for queues. *)
module type QUEUE = sig
  type 'a queue

  val empty : 'a queue

  val isEmpty : 'a queue -> bool

  val snoc : 'a queue -> 'a -> 'a queue

  val head : 'a queue -> 'a (* raises Empty if heap is empty *)

  val tail : 'a queue -> 'a queue (* raises Empty if heap is empty *)
end

(** Page 45 - Figure 5.3.
    Signature for double-ended queuese. *)
module type DEQUE = sig
  type 'a queue

  val empty : 'a queue

  val isEmpty : 'a queue -> bool

  (* insert, inspect, and remove the front element *)

  val cons : 'a -> 'a queue -> 'a queue

  val head : 'a queue -> 'a (* raises Empty if heap is empty *)

  val tail : 'a queue -> 'a queue (* raises Empty if heap is empty *)

  (* insert, inspect, and remove the rear element *)

  val snoc : 'a queue -> 'a -> 'a queue

  val last : 'a queue -> 'a (* raises Empty if heap is empty *)

  val init : 'a queue -> 'a queue (* raises Empty if heap is empty *)
end
