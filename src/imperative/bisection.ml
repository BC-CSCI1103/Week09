(* file: bisectionSqrt.ml
 * author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   Computing an approximation of the square root, Babylonian style.

   In this file, we compare recursive vs imperative versions.
 *)
let show i lo hi guess error =
  Lib.pfmt
    "try=%d: lo=%.10f, hi=%.10f, guess=%.10f, error=%.10f\n"
    i lo hi guess error

(* sqrt : float -> int -> float option
 *
 * The call (sqrt x n) will return an (option of) approximation
 * of the square root of x such that the square of the result is within
 * epsilon of x. The algorithm computes approximations iteratively and
 * returns None if it cannot compute the approximation within n iterations.
*)
let sqrt ?n:(n=50) x =
  let rec iterate i lo hi =
    match i > n with
    | true  -> None
    | false ->
      let guess = (lo +. hi) /. 2.0 in
      let guessSquared = guess ** 2.0 in
      let error = abs_float(x -. guessSquared) in
      let _ = show i lo hi guess error
      in
      match Lib.closeEnough guessSquared x with
      | true -> Some guess
      | false ->
        (match guessSquared < x with
		     | true  -> iterate (i + 1) guess hi
		     | false -> iterate (i + 1) lo guess)
  in
  iterate 1 0.0 x

(* Imperative version of bisection algorithm *)
let sqrt ?n:(n=50) x =
  let i = ref 1 in
  let lo = ref 0. in
  let hi = ref x in
  let guess = ref ((!lo +. !hi) /. 2.0) in
  let guessSquared = ref (!guess ** 2.0)
  in
  while !i <= n && not (Lib.closeEnough !guessSquared x) do
    let error = abs_float(x -. !guessSquared) in
    let _ = show !i !lo !hi !guess error
    in
    i := !i + 1;
    if !guessSquared < x then
      lo := !guess
    else
      hi := !guess;
    guess := (!lo +. !hi) /. 2.0;
    guessSquared := !guess ** 2.0
  done;
  if !i > n then None else Some !guess
