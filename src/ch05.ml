(** Chapter 5 - Fundamentals of Amortization *)

open Sigs

(** Page 43 - Figure 5.2.
    A common implementation of purely functional queues. *)
module BatchedQueue : QUEUE = struct
  type 'a queue = 'a list * 'a list

  let empty = ([], [])

  let isEmpty (f, _) = match f with [] -> true | _ -> false

  let checkf (f, r) = match (f, r) with [], r -> (List.rev r, []) | q -> q

  let snoc (f, r) x = checkf (f, x :: r)

  let head (f, _) = match f with [] -> raise Empty | x :: _ -> x

  let tail (f, r) = match f with [] -> raise Empty | _ :: f -> checkf (f, r)
end

(** Page 44 - Exercise 5.1
    This design can easily be extended to support the double-ended queue, or
    deque, abstraction, which allows reads and writes to both ends of the queue
    (see Figure 5.3). The invariant is updated to be symmetric in its treatment
    of f and r: both are required to be non-empty whenever the deque contains
    two or more elements. When one list becomes empty, we split the other list
    in half and reverse one of the halves.

    (a) Implement this version of the deques.
    (b) Prove that each operatino takes O(1) amortized time using the potential
        function phi(f, r) = abs(|f| - |r|), where abs is the absolute value
        function. *)
module BatchedDeque : DEQUE = struct
  type 'a queue = 'a list * 'a list

  let empty = ([], [])

  let isEmpty (f, _) = match f with [] -> true | _ -> false

  let rec splitAt n xs =
    match (n, xs) with
    | _, [] -> ([], [])
    | 1, x :: xs -> ([ x ], xs)
    | n, x :: xs ->
        let xs', ys = splitAt (n - 1) xs in
        (x :: xs', ys)

  let splitInHalf xs =
    let n = List.length xs in
    splitAt (n / 2) xs

  let checkf (f, r) =
    match (f, r) with
    | [], _ :: _ :: _ ->
        let r, f = splitInHalf r in
        (List.rev f, r)
    | _ :: _ :: _, [] ->
        let f, r = splitInHalf f in
        (f, List.rev r)
    | q -> q

  let cons x (f, r) = checkf (x :: f, r)

  let head (f, _) = match f with [] -> raise Empty | x :: _ -> x

  let tail (f, r) = match f with [] -> raise Empty | _ :: f -> checkf (f, r)

  let snoc (f, r) x = checkf (f, x :: r)

  let last (_, r) = match r with [] -> raise Empty | x :: _ -> x

  let init (f, r) = match r with [] -> raise Empty | _ :: r -> checkf (f, r)
end
