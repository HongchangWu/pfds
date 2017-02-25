(* Chapter 2 - Persistence *)

module L = List

(* Page 8 - Signature for stacks. *)
module type STACK =
  sig
    type 'a stack

    val empty   : 'a stack
    val isEmpty : 'a stack -> bool

    val cons    : 'a * 'a stack -> 'a stack
    val head    : 'a stack -> 'a
    val tail    : 'a stack -> 'a stack
  end

(* Page 8 - Implementation of stacks using the built-in type of lists. *)
module List : STACK =
  struct
    type 'a stack = 'a list

    let empty = []
    let isEmpty s = L.length s = 0

    let cons (x, s) = x :: s
    let head s = L.hd s
    let tail s = L.tl s
  end

(* Page 8 - Implementation of stacks using a custom datatype. *)
module CustomStack : STACK =
  struct
    type 'a stack = NIL | CONS of 'a * 'a stack

    let empty = NIL
    let isEmpty = function
      | NIL -> true
      | _ -> false

    let cons (x, s) = CONS (x, s)
    let head = function
      | NIL -> failwith "EMPTY"
      | CONS (x, s) -> x
    let tail = function
      | NIL -> failwith "EMPTY"
      | CONS (x, s) -> s
  end

let rec (++) xs ys =
  match xs with
  | [] -> ys
  | x :: xs' -> x :: xs ++ ys

let rec update = function
  | ([], _, _) -> failwith "SUBSCRIPT"
  | (x :: xs, 0, y) -> y :: xs
  | (x :: xs, i, y) -> update (xs, i - 1, y)

(* Page 11 - Exercise 2.1 *)
let rec suffixes = function
  | [] -> [[]]
  | _ :: xs' as xs -> xs :: suffixes xs'

(* Page 12 - Signature for sets. *)
module type SET =
  sig
    type elem
    type set

    val empty  : set
    val insert : elem * set -> set
    val member : elem * set -> bool
  end

(* Page 14 - Implementation of binary search trees as a functor. *)
module type ORDERED =
  (* a totally ordered type and its comparison functions *)
  sig
    type t

    val eq  : t * t -> bool
    val lt  : t * t -> bool
    val leq : t * t -> bool
  end

module UnbalancedSet (Element : ORDERED) : SET =
  struct
    type elem = Element.t
    type tree = E | T of tree * elem * tree
    type set = tree

    let empty = E

    let rec member = function
      | (x, E) -> false
      | (x, T (a, y, b)) ->
        if Element.lt (x, y) then member (x, a)
        else if Element.lt (y, x) then member (x, b)
        else true

    let rec insert = function
      | (x, E) -> T (E, x, E)
      | (x, (T (a, y, b) as s)) ->
        if Element.lt (x, y) then T (insert (x, a), y, b)
        else if Element.lt (y, x) then T (a, y, insert (x, b))
        else s
  end
