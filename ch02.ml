(* Chapter 2 - Persistence *)

(* Exercise 2.1 *)
let rec suffix = function
  | [] -> [[]]
  | _ :: xs' as xs -> xs :: suffix xs'
