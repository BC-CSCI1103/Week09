(* CSCI 1103 CS 1 Honors

   Comparing list-based and array-based versions of membership query.
*)

(* listMem : 'a -> 'a list -> bool *)
let rec listMem x xs =
  match xs with
  | [] -> false
  | y :: ys -> (x = y) || listMem x ys

(* arrayMem : 'a array -> 'a -> bool *)
let arrayMem a x =
  let exception Break
  in
  try
    for i = 0 to Array.length(a) - 1 do
      match a.(i) = x with
      | true  -> raise Break
      | false -> ()
    done;
    false
  with
  | Break -> true
