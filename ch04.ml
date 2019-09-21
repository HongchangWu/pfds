(** Chapter 4 - Lazy Evaluation *)

open Sigs

module Stream : STREAM =
struct
  type 'a streamCell = Nil | Cons of 'a * 'a stream
  and 'a stream = 'a streamCell Lazy.t

  let rec (++) s t =
    match Lazy.force s with
    | Nil -> t
    | Cons (x, s) -> lazy (Cons (x, s ++ t))
  let rec take n s =
    if n = 0 then lazy Nil
    else match Lazy.force s with
      | Nil -> lazy Nil
      | Cons (x, s) -> lazy (Cons (x, take (n - 1) s))
  let drop n s =
    let rec drop' n s =
      match Lazy.force s with
      | Nil -> Nil
      | Cons (_, s) -> drop' (n - 1) s
    in
    if n = 0 then s
    else lazy (drop' n s) (* delay the evaluation of drop', which is monolithic *)
  let reverse s =
    let rec reverse' s r =
      match Lazy.force s with
      | Nil -> r
      | Cons (x, s) -> reverse' s (Cons (x, lazy r))
    in
    lazy (reverse' s Nil) (* delay the evaluation of reverse', which is monolithic *)

  (** Page 37 - Exercise 4.2
      Implement insertion sort on streams. Show that extracting the first k elements of
      sort xs takes only O(n*k) time, where n is the length of xs, rather than O(n^2),
      as might be expected of insertion sort.
  *)
  let insertionSort
        (type a)
        (module Ord : ORDERED with type t = a)
        (xs : a stream) : a stream =
    let rec insert x ys =
      match Lazy.force ys with
      | Nil ->
        lazy (Cons (x, lazy Nil))
      | Cons (y, ys) ->
        if Ord.lt x y then
          lazy (Cons (x, lazy (Cons (y, ys))))
        else
          lazy (Cons (y, insert x ys))
    in
    let rec insertionSort' xs =
      match Lazy.force xs with
      | Nil -> lazy Nil
      | Cons (x, xs) -> insert x @@ insertionSort' xs
    in
    insertionSort' xs
end
