(* CSCI 1103 CS 1 Honors

   Comparing list-based and array-based versions of summing.
*)

(* listSum : int list -> int, this version is not tail-recursive *)
let rec listSum ns =
  match ns with
  | [] -> 0
  | n :: ns -> n + listSum ns

(* A tail-recursive version of listSum *)
let listSum ns =
  let rec repeat ns answer =
    match ns with
    | [] -> answer
    | n :: ns -> repeat ns (n + answer)
  in
  repeat ns 0

(* arraySum : int array -> int *)
let arraySum a =
  let sum = ref 0
  in
  for i = 0 to Array.length(a) - 1 do
    sum := !sum + a.(i)
  done;
  !sum
