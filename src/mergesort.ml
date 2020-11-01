(* CSCI 1103 CS 1 Honors

   Comparing list-based and array-based versions of merge sort.
*)

(* mergesort is O(N log N) in both the worst and averages cases.

   merge : 'a list -> 'a list -> 'a list

   NB: Invariant: In (merge xs ys), xs and ys must arrive in ascending order.
*)
let rec merge xs ys =
  match (xs, ys) with
  | ([], []) -> []
  | (zs, []) | ([], zs) -> zs
  | (v :: vs, z :: zs) ->
    (match compare v z <= 0 with
     | true  -> v :: merge vs ys
     | false -> z :: merge xs zs)

(* divide : 'a list -> 'a list * 'a list *)
let divide xs =
  let rec repeat n xs =
    match (n = 0, xs) with
    | (true, _) -> ([], xs)
    | (false, []) -> failwith "divide: this can't happen"
    | (false, x :: xs) ->
      let (left, right) = repeat (n - 1) xs
      in
      (x :: left, right)
  in
  repeat ((List.length xs) / 2) xs

(* mergesort : 'a list -> 'a list
*)
let rec mergesort xs =
  match xs with
  | [] | [_] -> xs
  | _ ->
    let (left, right) = divide xs
    in
    merge (mergesort left) (mergesort right)

(*********************************************************************)

(* precondition: a[lo .. mid] and a[mid+1 .. hi] are sorted subarrays
   postcondition: a[lo .. hi] is sorted
*)
(* merge : 'a list -> 'a list -> int -> int -> int -> unit *)
let merge a aux lo mid hi =
  for k = lo to hi do aux.(k) <- a.(k) done; (* copy a to aux *)
  let i = ref lo in
  let j = ref (mid + 1)
  in
  for k = lo to hi do
    match !i > mid with
    | true  -> a.(k) <- aux.(!j); j := !j + 1
    | false ->
      (match !j > hi with
       | true  -> a.(k) <- aux.(!i); i := !i + 1
       | false ->
         (match aux.(!j) < aux.(!i) with
          | true  -> a.(k) <- aux.(!j); j := !j + 1
          | false -> a.(k) <- aux.(!i); i := !i + 1))
  done

let rec sort a aux lo hi =
  match hi <= lo with
  | true  -> ()
  | false ->
    let mid = lo + (hi - lo) / 2
    in
    sort a aux lo mid;
    sort a aux (mid + 1) hi;
    merge a aux lo mid hi

let arrayMergeSort a =
  let aux = Array.make (Array.length a) 0
  in
  sort a aux 0 (Array.length a - 1)

let a = [|5; 4; 8; 1; 9; 6; 10; 3; 2|]
