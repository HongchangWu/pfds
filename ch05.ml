(** Chapter 5 - Fundamentals of Amortization *)

open Sigs

(** Page 43 - Figure 5.2.
    A common implementation of purely functional queues. *)
module BatchedQueue : QUEUE =
struct
  type 'a queue = 'a list * 'a list

  let empty = ([], [])
  let isEmpty (f, _) = match f with
    | [] -> true
    | _ -> false

  let checkf (f, r) = match (f, r) with
    | ([], r) -> (List.rev r, [])
    | q -> q

  let snoc (f, r) x = checkf (f, x :: r)

  let head (f, _) = match f with
    | [] -> raise Empty
    | x :: _ -> x

  let tail (f, r) = match f with
    | [] -> raise Empty
    | _ :: f -> checkf (f, r)
end
