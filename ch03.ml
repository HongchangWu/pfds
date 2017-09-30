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

  (** Page 19 - Exercise 3.2
      Define [insert] directly rather than via a call to [merge]
  *)
  let rec insert (x, t) =
    match t with
    | E ->
      T (1, x, E, E)
    | T (r, y, a, b) ->
      if Elem.leq (x, y)
      then makeT (x, a, insert (y, b))
      else makeT (y, a, insert (x, b))

  (** Page 19 - Exercise 3.3
      Implement a function [fromList] of type [Element.t list -> heap] that
      produces a lefitst heap from an unordered list of elemetns by first converting
      each element into a singleton heap and then mergin the heaps until only one
      heap remains. Instead of merging the heaps in one right-to-left or left-to-right
      pass using [foldr] or [foldl], merge the heaps in [log n] passes, where each pass
      merges adjacent pairs of heaps. Show that [fromList] takes only [O(n)] time.
  *)
  let fromList : Element.t list -> heap =
    fun xs ->
      let rec mergeAdjacent = function
        | [] -> []
        | [t] -> [t]
        | t1 :: t2 :: ts ->
          merge (t1, t2) :: mergeAdjacent ts
      in
      let rec go = function
        | [] -> failwith "Empty"
        | [t] -> t
        | ts -> go (mergeAdjacent ts)
      in
      xs
      |> List.map (fun x -> T (1, x, E, E))
      |> go
end
