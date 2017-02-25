(** Chapter 2 - Persistence *)

module L = List

(** Page 8 - Signature for stacks. *)
module type Stack =
  sig
    type 'a stack

    val empty   : 'a stack
    val isEmpty : 'a stack -> bool

    val cons    : 'a * 'a stack -> 'a stack
    val head    : 'a stack -> 'a
    val tail    : 'a stack -> 'a stack
  end

(** Page 8 - Implementation of stacks using the built-in type of lists. *)
module List : Stack =
  struct
    type 'a stack = 'a list

    let empty = []
    let isEmpty s = L.length s = 0

    let cons (x, s) = x :: s
    let head s = L.hd s
    let tail s = L.tl s
  end

(** Page 8 - Implementation of stacks using a custom datatype. *)
module CustomStack : Stack =
  struct
    type 'a stack = Nil | Cons of 'a * 'a stack

    let empty = Nil
    let isEmpty = function
      | Nil -> true
      | _ -> false

    let cons (x, s) = Cons (x, s)
    let head = function
      | Nil -> failwith "Empty"
      | Cons (x, s) -> x
    let tail = function
      | Nil -> failwith "Empty"
      | Cons (x, s) -> s
  end

let rec (++) xs ys =
  match xs with
  | [] -> ys
  | x :: xs' -> x :: xs ++ ys

let rec update = function
  | ([], _, _) -> failwith "Subscript"
  | (x :: xs, 0, y) -> y :: xs
  | (x :: xs, i, y) -> update (xs, i - 1, y)

(** Page 11 - Exercise 2.1
    Write a function suffixes of type ['a list -> 'a list list] that
    takes a list [xs] and returns a list of all the suffixes of [xs]
    in decreasing order of length. For example,
    [{
      suffixes [1;2;3;4] = [[1;2;3;4]; [2;3;4]; [3;4]; [4]; []]
    }]
    Show that the resulting list of suffixes can be generated in [O(n)]
    time and represented in [O(n)] space.
 *)
let rec suffixes = function
  | [] -> [[]]
  | _ :: xs' as xs -> xs :: suffixes xs'

(** Page 12 - Signature for sets. *)
module type Set =
  sig
    type elem
    type set

    val empty  : set
    val insert : elem * set -> set
    val member : elem * set -> bool
  end

(** Page 14 - Implementation of binary search trees as a functor. *)
module type Ordered =
  (* a totally ordered type and its comparison functions *)
  sig
    type t

    val eq  : t * t -> bool
    val lt  : t * t -> bool
    val leq : t * t -> bool
  end

module UnbalancedSet (Element : Ordered) : Set with type elem = Element.t =
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

    (** Page 14 - Exercise 2.2
        In the worse case, [member] performs approximately [2d] comparisons,
        where [d] is the depth of the tree. Rewrite [member] to take no
        more than [d + 1] comparisons by keeping track of a candidate
        element that {i might} be equal to the query element (say, the last
        element for which [<] returned false or [<=] returned true) and
        checking for equality only when you hit the bottom of the tree.
     *)
    let member (x, s) =
      let rec go z = function
        | E -> z
        | T (a, y, b) ->
          if Element.lt (x, y)
          then go z a
          else go (Some y) b
      in
      match go None s with
      | Some y -> x = y
      | None -> false

    let rec insert = function
      | (x, E) -> T (E, x, E)
      | (x, (T (a, y, b) as s)) ->
        if Element.lt (x, y) then T (insert (x, a), y, b)
        else if Element.lt (y, x) then T (a, y, insert (x, b))
        else s

    (** Page 15 - Exercise 2.3
        Inserting an existing element into a binary search tree copies
        the entire search path even though the copied nodes are indistinguishable
        from the originals. Rewrite [insert] using exceptions to avoid
        this copying. Establish only one handler per insertion rather
        than one handler per iteration.
     *)
    let insert (x, s) =
      let rec go = function
        | (x, E) -> T (E, x, E)
        | (x, T (a, y, b)) ->
        if Element.lt (x, y) then T (insert (x, a), y, b)
        else if Element.lt (y, x) then T (a, y, insert (x, b))
        else failwith "Found"
      in
      try
        go (x, s)
      with Failure _ ->
        s

    (** Page 15 - Exercise 2.4
        Combine the dieas of the previous two exercises to obtain a
        version of [insert] that performs no necessary copying and uses
        no more than [d + 1] comparisons
     *)
    let insert (x, s) =
      let rec go z = function
        | E ->
          begin
            match z with
            | Some y when x = y ->
              failwith "Found"
            | _ ->
              T (E, x, E)
          end
        | T (a, y, b) ->
          if Element.lt (x, y)
          then T (go z a, y, b)
          else T (a, y, go (Some y) b)
      in
      try
        go None s
      with Failure _ ->
        s
  end
