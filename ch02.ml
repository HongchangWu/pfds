(* Chapter 2 - Persistence *)

module L = List

(* Signature for stacks. *)
module type STACK =
  sig
    type 'a stack

    val empty   : 'a stack
    val isEmpty : 'a stack -> bool

    val cons    : 'a * 'a stack -> 'a stack
    val head    : 'a stack -> 'a
    val tail    : 'a stack -> 'a stack
  end

(* Implementation of stacks using the built-in type of lists. *)
module List : STACK =
  struct
    type 'a stack = 'a list

    let empty = []
    let isEmpty s = L.length s = 0

    let cons (x, s) = x :: s
    let head s = L.hd s
    let tail s = L.tl s
  end

(* Implementation of stacks using a custom datatype. *)
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

(* Exercise 2.1 *)
let rec suffixes = function
  | [] -> [[]]
  | _ :: xs' as xs -> xs :: suffixes xs'
