module FArray = Float.Array

module Sample = struct
  include FArray

  let ( .%{} ) = get
end

type t =
  { current : floatarray
  ; alpha : floatarray
  }

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
let clamp_add a b = clamp @@ (a +. b)

let alpha dimension =
  let phi = phi_approx dimension in
  FArray.init dimension (fun i -> recip (phi ** Float.of_int (i + 1)))
;;

let create ~dimension =
  if dimension < 1
  then failwith "Low_discrepancy_sequence.create: expected dimension >= 1";
  let alpha = alpha dimension in
  let current = FArray.copy alpha in
  { current; alpha }
;;

let split_at t n =
  let f c a = clamp_add c (a *. Float.of_int n) in
  let next = FArray.map2 f t.current t.alpha in
  let suffix = { alpha = t.alpha; current = next } in
  t, suffix
;;

let step t =
  let sample = t.current in
  (* This inline saves a significant chunk of allocation;
     otherwise map2 calls clamp_add via a closure and winds up having to box floats. *)
  let next = (FArray.map2 [@inlined]) clamp_add t.current t.alpha in
  let t' = { t with current = next } in
  t', sample
;;

module Simple = struct
  type t = { alpha : floatarray } [@@unboxed]

  let create ~dimensions =
    if dimensions < 1
    then failwith "Low_discrepancy_sequence.create: expected dimension >= 1";
    { alpha = alpha dimensions }
  ;;

  let get t ~offset ~dimension =
    let a = FArray.get t.alpha dimension in
    clamp @@ (a *. float_of_int (1 + offset))
  ;;
end
