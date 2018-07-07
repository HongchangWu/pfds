(** Chapter 3 - Some Familiar Data Structures in a Functional Setting *)

open Sig

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
    | E -> raise Empty
    | (T (_, x, _, _)) -> x
  let deleteMin = function
    | E -> raise Empty
    | (T (_, _, a, b)) -> merge (a, b)

  (** Page 19 - Exercise 3.2
      Define [insert] directly rather than via a call to [merge]
  *)
  let rec insert (x, t) =
    match t with
    | E ->
      T (1, x, E, E)
    | T (_, y, a, b) ->
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
        | [] -> raise Empty
        | [t] -> t
        | ts -> go (mergeAdjacent ts)
      in
      xs
      |> List.map (fun x -> T (1, x, E, E))
      |> go
end

(** Page 19 - Exercise 3.4
    Weight-biased leftist heaps are an alternative to leftist heaps that replace
    the leftist property with the weight-biased leftist property: the size of
    any child is at least as large as the size of its right siblings.
*)
module WeightLeftistHeap (Element : Ordered) : Heap with module Elem = Element =
struct
  module Elem = Element

  type heap = E | T of int * Elem.t * heap * heap

  let empty = E
  let isEmpty = function
    | E -> true
    | _ -> false

  let size = function
    | E -> 0
    | T (n, _, _, _) -> n
  let makeT (x, a, b) =
    if size a >= size b
    then T (size a + size b, x, a, b)
    else T (size a + size b, x, b, a)

  let rec merge = function
    | (h, E) -> h
    | (E, h) -> h
    | ((T (n1, x, a1, b1) as h1), (T (n2, y, a2, b2) as h2)) ->
      if Elem.leq (x, y)
      then
        if size a1 + size b1 >= n2
        then T (n1 + n2, x, merge (a1, a2), h2)
        else T (n1 + n2, x, h2, merge (a1, a2))
      else
        begin
          if size a2 + size b2 >= n1
          then T (n1 + n2, y, merge (a2, b2), h1)
          else T (n1 + n2, y, h1, merge (a2, b2))
        end

  let rec insert (x, t) =
    match t with
    | E ->
      T (1, x, E, E)
    | T (n, y, a, b) ->
      if Elem.leq (x, y)
      then
        if size a = size b
        then T (n + 1, x, insert (y, a), b)
        else T (n + 1, x, a, insert (y, b))
      else
        begin
          if size a = size b
          then T (n + 1, y, insert (x, a), b)
          else T (n + 1, y, a, insert (x, b))
        end

  let insert (x, h) = merge (T (1, x, E, E), h)

  let findMin = function
    | E -> raise Empty
    | (T (_, x, _, _)) -> x

  let deleteMin = function
    | E -> raise Empty
    | (T (_, _, a, b)) -> merge (a, b)
end

(** Page 24 - Binomial heaps.
*)
module BinomialHeap (Element : Ordered) : Heap with module Elem = Element =
struct
  module Elem = Element

  type tree = Node of int * Element.t * tree list
  type heap = tree list

  let empty = []
  let isEmpty = function [] -> true | _ -> false

  let rank (Node (r, _, _)) = r
  let root (Node (_, x, _)) = x
  let link ((Node (r, x1, c1) as t1), (Node (_, x2, c2) as t2)) =
    if Elem.leq (x1, x2)
    then Node (r + 1, x1, t2 :: c1)
    else Node (r + 1, x2, t1 :: c2)
  let rec insTree = function
    | (t, []) -> [t]
    | (t, (t' :: ts' as ts)) ->
      if rank t < rank t' then t :: ts else insTree (link (t, t'), ts')

  let insert (x, ts) = insTree (Node (0, x, []), ts)
  let rec merge = function
    | (ts1, []) -> ts1
    | ([], ts2) -> ts2
    | ((t1 :: ts1' as ts1), (t2 :: ts2' as ts2)) ->
      if rank t1 < rank t2 then t1 :: merge (ts1', ts2)
      else if rank t2 < rank t1 then t2 :: merge (ts1, ts2')
      else insTree (link (t1, t2), merge (ts1', ts2'))

  let rec removeMinTree = function
    | [] -> raise Empty
    | t :: ts ->
      let t', ts' = removeMinTree ts in
      if Elem.leq (root t, root t') then (t, ts) else (t', t :: ts')

  let findMin ts =
    let (t, _) = removeMinTree ts in
    root t
  let deleteMin ts =
    let Node (_, _, ts1), ts2 = removeMinTree ts in
    merge (List.rev ts1, ts2)
end

(** Page 28 - Leftist heaps. *)
module RedBlackSet (Element : Ordered) : Set with type elem = Element.t =
struct
  type elem = Element.t

  type color = R | B
  type tree = E | T of color * tree * elem * tree
  type set = tree

  let empty = E

  let rec member = function
    | (_, E) -> false
    | (x, T (_, a, y, b)) ->
      if Element.lt (x, y) then member (x, a)
      else if Element.lt (y, x) then member (x, b)
      else true

  let balance = function
    | (B, T (R, T (R, a, x, b), y, c), z, d)
    | (B, T (R, a, x, T (R, b, y, c)), z, d)
    | (B, a, x, T (R, T (R, b, y, c), z, d))
    | (B, a, x, T (R, b, y, T (R, c, z, d))) ->
      T (R, T (B, a, x, b), y, T (B, c, z, d))
    | (color, a, x, b) ->
      T (color, a, x, b)

  let insert (x, s) =
    let rec ins = function
      | E -> T (R, E, x, E)
      | T (color, a, y, b) as s ->
        if Element.lt (x, y) then balance (color, ins a, y, b)
        else if Element.lt (y, x) then balance (color, a, y, ins b)
        else s
    in
    match ins s with
    | E -> E
    | T (_, a, y, b) -> T (B, a, y, b)
end
