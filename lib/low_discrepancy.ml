open Base
module FArray = Caml.Float.Array
module Sample = struct include FArray end

type t = {mutable current: floatarray; alpha: floatarray}

let phi_approx d =
  let d' = 1 // (d + 1) in
  let open Float.O in
  let refine x = (1.0 + x) ** d' in
  let rec loop prev =
    let next = refine prev in
    if prev = next then prev else loop next in
  loop 2.0

let clamp x = Float.mod_float (0.5 +. x) 1.0

let create dimensions =
  let phi = phi_approx dimensions in
  let alpha = FArray.init dimensions (fun i -> 1.0 /. (phi **. Float.of_int (i + 1))) in
  {current= alpha; alpha}

let step t =
  let c = t.current in
  t.current <- FArray.map2 (fun a b -> clamp (a +. b)) t.alpha c ;
  c

let split_at t n =
  let f c a = clamp @@ (c +. (a *. Float.of_int n)) in
  let next = FArray.map2 f t.current t.alpha in
  let suffix = {alpha= t.alpha; current= next} in
  (t, suffix)
