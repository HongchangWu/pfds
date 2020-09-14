(** Chapter 3 - Some Familiar Data Structures in a Functional Setting *)

open Sigs

(** Page 32 - Leftist heaps. *)
module LeftistHeap (Element : ORDERED) : HEAP with module Elem = Element = struct
  module Elem = Element

  type heap =
    | E
    | T of int * Elem.t * heap * heap

  let empty = E

  let isEmpty = function
    | E -> true
    | _ -> false
  ;;

  let rank = function
    | E -> 0
    | T (r, _, _, _) -> r
  ;;

  let makeT x a b =
    if rank a >= rank b then T (rank b + 1, x, a, b) else T (rank a + 1, x, b, a)
  ;;

  let rec merge a b =
    match a, b with
    | h, E -> h
    | E, h -> h
    | (T (_, x, a1, b1) as h1), (T (_, y, a2, b2) as h2) ->
      if Elem.leq x y then makeT x a1 (merge b1 h2) else makeT y a2 (merge h1 b2)
  ;;

  let insert x h = merge (T (1, x, E, E)) h

  let findMin = function
    | E -> raise Empty
    | T (_, x, _, _) -> x
  ;;

  let deleteMin = function
    | E -> raise Empty
    | T (_, _, a, b) -> merge a b
  ;;

  (** Page 19 - Exercise 3.2
      Define [insert] directly rather than via a call to [merge]
  *)
  let rec insert x t =
    match t with
    | E -> T (1, x, E, E)
    | T (_, y, a, b) ->
      if Elem.leq x y then makeT x a (insert y b) else makeT y a (insert x b)
  ;;

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
      | [ t ] -> [ t ]
      | t1 :: t2 :: ts -> merge t1 t2 :: mergeAdjacent ts
    in
    let rec go = function
      | [] -> raise Empty
      | [ t ] -> t
      | ts -> go (mergeAdjacent ts)
    in
    xs |> List.map (fun x -> T (1, x, E, E)) |> go
 ;;
end

(** Page 19 - Exercise 3.4
    Weight-biased leftist heaps are an alternative to leftist heaps that replace
    the leftist property with the weight-biased leftist property: the size of
    any child is at least as large as the size of its right siblings.
*)
module WeightLeftistHeap (Element : ORDERED) : HEAP with module Elem = Element = struct
  module Elem = Element

  type heap =
    | E
    | T of int * Elem.t * heap * heap

  let empty = E

  let isEmpty = function
    | E -> true
    | _ -> false
  ;;

  let size = function
    | E -> 0
    | T (n, _, _, _) -> n
  ;;

  let makeT (x, a, b) =
    if size a >= size b then T (size a + size b, x, a, b) else T (size a + size b, x, b, a)
  ;;

  let rec merge a b =
    match a, b with
    | h, E -> h
    | E, h -> h
    | (T (n1, x, a1, b1) as h1), (T (n2, y, a2, b2) as h2) ->
      if Elem.leq x y
      then
        if size a1 + size b1 >= n2
        then T (n1 + n2, x, merge a1 a2, h2)
        else T (n1 + n2, x, h2, merge a1 a2)
      else if size a2 + size b2 >= n1
      then T (n1 + n2, y, merge a2 b2, h1)
      else T (n1 + n2, y, h1, merge a2 b2)
  ;;

  let rec insert x t =
    match t with
    | E -> T (1, x, E, E)
    | T (n, y, a, b) ->
      if Elem.leq x y
      then
        if size a = size b
        then T (n + 1, x, insert y a, b)
        else T (n + 1, x, a, insert y b)
      else if size a = size b
      then T (n + 1, y, insert x a, b)
      else T (n + 1, y, a, insert x b)
  ;;

  let insert x h = merge (T (1, x, E, E)) h

  let findMin = function
    | E -> raise Empty
    | T (_, x, _, _) -> x
  ;;

  let deleteMin = function
    | E -> raise Empty
    | T (_, _, a, b) -> merge a b
  ;;
end

(** Page 24 - Binomial heaps. *)
module BinomialHeap (Element : ORDERED) : HEAP with module Elem = Element = struct
  module Elem = Element

  type tree = Node of int * Element.t * tree list
  type heap = tree list

  let empty = []

  let isEmpty = function
    | [] -> true
    | _ -> false
  ;;

  let rank (Node (r, _, _)) = r
  let root (Node (_, x, _)) = x

  let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
    if Elem.leq x1 x2 then Node (r + 1, x1, t2 :: c1) else Node (r + 1, x2, t1 :: c2)
  ;;

  let rec insTree t1 t2 =
    match t1, t2 with
    | t, [] -> [ t ]
    | t, (t' :: ts' as ts) ->
      if rank t < rank t' then t :: ts else insTree (link t t') ts'
  ;;

  let insert x ts = insTree (Node (0, x, [])) ts

  let rec merge ts1 ts2 =
    match ts1, ts2 with
    | ts1, [] -> ts1
    | [], ts2 -> ts2
    | (t1 :: ts1' as ts1), (t2 :: ts2' as ts2) ->
      if rank t1 < rank t2
      then t1 :: merge ts1' ts2
      else if rank t2 < rank t1
      then t2 :: merge ts1 ts2'
      else insTree (link t1 t2) (merge ts1' ts2')
  ;;

  let rec removeMinTree = function
    | [] -> raise Empty
    | t :: ts ->
      let t', ts' = removeMinTree ts in
      if Elem.leq (root t) (root t') then t, ts else t', t :: ts'
  ;;

  let findMin ts =
    let t, _ = removeMinTree ts in
    root t
  ;;

  let deleteMin ts =
    let Node (_, _, ts1), ts2 = removeMinTree ts in
    merge (List.rev ts1) ts2
  ;;
end

(** Page 28 - Leftist heaps. *)
module RedBlackSet (Element : ORDERED) : SET with type elem = Element.t = struct
  type elem = Element.t

  type color =
    | R
    | B

  type tree =
    | E
    | T of color * tree * elem * tree

  type set = tree

  let empty = E

  let rec member x s =
    match x, s with
    | _, E -> false
    | x, T (_, a, y, b) ->
      if Element.lt x y then member x a else if Element.lt y x then member x b else true
  ;;

  let balance = function
    | B, T (R, T (R, a, x, b), y, c), z, d
    | B, T (R, a, x, T (R, b, y, c)), z, d
    | B, a, x, T (R, T (R, b, y, c), z, d)
    | B, a, x, T (R, b, y, T (R, c, z, d)) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
    | color, a, x, b -> T (color, a, x, b)
  ;;

  let insert x s =
    let rec ins = function
      | E -> T (R, E, x, E)
      | T (color, a, y, b) as s ->
        if Element.lt x y
        then balance (color, ins a, y, b)
        else if Element.lt y x
        then balance (color, a, y, ins b)
        else s
    in
    match ins s with
    | E -> E
    | T (_, a, y, b) -> T (B, a, y, b)
  ;;

  (** Page 28 - Exercise 3.9
      Write a function fromOrdList of type elem list -> tree that converts a sorted list
      with no duplicates into a red-black tree. Your function should run in O(n) time
,  *)
  let fromOrdList (xs : elem list) : tree =
    let rec splitAt n = function
      | xs when n <= 0 -> [], xs
      | [] -> [], []
      | x :: xs ->
        let xs', xs'' = splitAt (n - 1) xs in
        x :: xs', xs''
    in
    let rec go color = function
      | [] -> E
      | xs ->
        (match splitAt (List.length xs / 2) xs with
        | us, [] ->
          (* for pattern-matching completeness, impossible in practice *)
          go color us
        | us, x :: vs ->
          let color' =
            match color with
            | B -> R
            | R -> B
          in
          let a = go color' us in
          let b = go color' vs in
          T (color, a, x, b))
    in
    go B xs
  ;;

  (** Page 28 - Exercise 3.10
      The balance function currently performs several unnecessary tests. For example,
      when the ins function recurses on the left child, there is no need for balance
      to test for red-red violations involving the right child.

      (a) Split balance into two functions, lbalance and rbalance, that test for
          violations involving the left child and right child, respectively. Replace
          the calls to balance in ins with calls to either lbalance and rbalance.
      (b) Extending the same logic one step further, one of the remaining tests on the
          grandchildren is also unnecessary. Rewrite ins so that it never tests the color
          of nodes not on the search path.
  *)
  type direction =
    | Left
    | Right

  let lbalance = function
    | B, T (R, T (R, a, x, b), y, c), z, d | B, T (R, a, x, T (R, b, y, c)), z, d ->
      T (R, T (B, a, x, b), y, T (B, c, z, d))
    | color, a, x, b -> T (color, a, x, b)
  ;;

  let rbalance = function
    | B, a, x, T (R, T (R, b, y, c), z, d) | B, a, x, T (R, b, y, T (R, c, z, d)) ->
      T (R, T (B, a, x, b), y, T (B, c, z, d))
    | color, a, x, b -> T (color, a, x, b)
  ;;

  let llbalance = function
    | B, T (R, T (R, a, x, b), y, c), z, d -> T (R, T (B, a, x, b), y, T (B, c, z, d))
    | color, a, x, b -> T (color, a, x, b)
  ;;

  let lrbalance = function
    | B, T (R, a, x, T (R, b, y, c)), z, d -> T (R, T (B, a, x, b), y, T (B, c, z, d))
    | color, a, x, b -> T (color, a, x, b)
  ;;

  let rlbalance = function
    | B, a, x, T (R, T (R, b, y, c), z, d) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
    | color, a, x, b -> T (color, a, x, b)
  ;;

  let rrbalance = function
    | B, a, x, T (R, b, y, T (R, c, z, d)) -> T (R, T (B, a, x, b), y, T (B, c, z, d))
    | color, a, x, b -> T (color, a, x, b)
  ;;

  let insert x s =
    let rec ins = function
      | E -> None, T (R, E, x, E)
      | T (color, a, y, b) as s ->
        if Element.lt x y
        then (
          let d, a' = ins a in
          match d with
          | Some Left -> Some Left, llbalance (color, a', y, b)
          | Some Right -> Some Left, lrbalance (color, a', y, b)
          | None -> Some Left, lbalance (color, a', y, b))
        else if Element.lt y x
        then (
          let d, b' = ins b in
          match d with
          | Some Left -> Some Right, rlbalance (color, a, y, b')
          | Some Right -> Some Right, rrbalance (color, a, y, b')
          | None -> Some Right, rbalance (color, a, y, b'))
        else None, s
    in
    match ins s with
    | _, E -> E
    | _, T (_, a, y, b) -> T (B, a, y, b)
  ;;
end
