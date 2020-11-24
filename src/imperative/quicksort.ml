(* CSCI 1103 CS 1 Honors

   Comparing list-based and array-based versions of quick sort.
*)

(* quicksort is O(N^2) in the worst case and O(N log N) in the
   average case.
*)
let rec split pivot xs =
  match xs with
  | [] -> ([], [])
  | y :: ys ->
    let (smaller, larger) = split pivot ys
    in
    (match compare y pivot <= 0 with
     | true  -> (y :: smaller, larger)
     | false -> (smaller, y :: larger))

(* quicksort : 'a list -> 'a list *)
let rec quicksort xs =
  match xs with
  | [] | [_] -> xs
  | pivot :: ys ->
    let (smaller, larger) = split pivot ys
    in
    (quicksort smaller) @ pivot :: (quicksort larger)
