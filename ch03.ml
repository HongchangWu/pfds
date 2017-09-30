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

(** Page 32 - Leftist heaps. *)
module LeftistHeap (Element : Ordered) : Heap with module Elem = Element =
struct
  module Elem = Element

  type heap = E | T of int * Elem.t * heap * heap

  let empty = E
  let isEmpty = function
    | E -> true
    | _ -> false

  let rank = function
    | E -> 0
    | T (r, _, _, _) -> r
  let makeT (x, a, b) =
    if rank a >= rank b
    then T (rank b + 1, x, a, b)
    else T (rank a + 1, x, b, a)

  let rec merge = function
    | (h, E) -> h
    | (E, h) -> h
    | ((T (_, x, a1, b1) as h1), (T (_, y, a2, b2) as h2)) ->
      if Elem.leq (x, y)
      then makeT (x, a1, merge (b1, h2))
      else makeT (y, a2, merge (h1, b2))

  let insert (x, h) = merge (T (1, x, E, E), h)

  let findMin = function
    | E -> failwith "Empty"
    | (T (_, x, a, b)) -> x
  let deleteMin = function
    | E -> failwith "Empty"
    | (T (_, x, a, b)) -> merge (a, b)
end
