module FArray = Float.Array

type t = { alpha : floatarray } [@@unboxed]

let recip x = 1.0 /. x

(* approximate solution to x^(d+1) = x + 1 *)
let phi_approx d =
  assert (d >= 1);
  let d' = recip (float_of_int d +. 1.0) in
  let refine x = (1.0 +. x) ** d' in
  let rec loop x =
    let x' = refine x in
    if x = x' then x else loop x'
  in
  loop 2.0
;;

let fractional x = x -. Float.trunc x
let clamp x = fractional (0.5 +. x)

let alpha dimension =
  let phi = phi_approx dimension in
  FArray.init dimension (fun i -> recip (phi ** Float.of_int (i + 1)))
;;

let create ~dimension =
  if dimension < 1
  then failwith "Low_discrepancy_sequence.create: expected dimension >= 1";
  { alpha = alpha dimension }
;;

let get t ~offset ~dimension =
  let a = FArray.get t.alpha dimension in
  clamp @@ (a *. float_of_int (1 + offset))
;;
