(* CSCI 1103 CS 1 Honors

   Comparing list-based and array-based versions of range generation.
*)

(* listRange : int -> int list, NB: this version is tail-recursive. *)
let listRange n =
  let rec repeat n answer =
    match n < 0 with
    | true  -> answer
    | false -> repeat (n - 1) (n :: answer)
  in
  repeat (n - 1) []

(* arrayRange : int -> int array *)
let arrayRange n =
  let a = Array.make n 0
  in
  for i = 0 to n - 1 do
    a.(i) <- i
  done;
  a
