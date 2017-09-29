(** Chapter 3 - Some Familiar Data Structures in a Functional Setting *)

open Common

(** Page 18 - Signature for heaps (priority queues). *)
module type Heap =
  sig
    module Elem : Ordered

    type heap

    val empty     : heap
    val isEmpty   : heap -> bool

    val insert    : Elem.t * heap -> heap
    val merge     : heap * heap -> heap

    val findMin   : heap -> Elem.t (* raises Empty if heap is empty *)
    val deleteMin : heap -> heap   (* raises Empty if heap is empty *)
  end
