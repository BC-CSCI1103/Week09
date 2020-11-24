(* CSCI 1103 CS 1 Honors

   Comparing list-based and array-based versions of insertion sort.
*)

(* insert 'a -> a' list -> 'a List

   In the call (insert x xs), If xs respects the invariant that
   it contains keys in sorted order, the (insert x xs) returns a
   list in sorted order. This function is unit time in the best
   case and O(N) in the average and worst cases.
*)
let rec insert x xs =
  match xs with
  | [] -> [x]
  | y :: ys ->
    (match compare x y <= 0 with
     | true  -> x :: xs
     | false -> y :: insert x ys)

(* listInsertionSort : 'a list -> 'a list

   insertionsort is O(N) in the best case --- keys already in
   almost sorted order --- and O(N^2) in both the average and
   worst cases.
*)
let rec listInsertionSort xs =
  match xs with
  | [] -> []
  | y :: ys -> insert y (listInsertionSort ys)

(* insertionsort : 'a list -> 'a list

   This is an alternative implementation of insertionsort.
   List.fold_right builds the required N-way compositions
   of insert.
*)
let listInsertionSort xs = List.fold_right insert xs []


(* exchange : 'a array -> int -> int -> unit *)
let exchange a i j =
  let temp = a.(i)
  in
  a.(i) <- a.(j);
  a.(j) <- temp

(* sort : 'a list -> 'a list *)
let arrayInsertionSort a =
  let n = Array.length a
  in
  for i = 1 to (n - 1) do
    let j = ref i
    in
    while !j > 0 && a.(!j) < a.(!j - 1) do
      exchange a !j (!j - 1);
      j := !j - 1
    done
  done

let a = [|5; 4; 8; 1; 9; 6; 10; 3; 2|]
