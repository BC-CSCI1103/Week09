(* fresh : unit -> int *)
let fresh =
  let count = ref 0
  in
  (fun () ->
    let result = !count
    in
    count := !count + 1;
    result)
