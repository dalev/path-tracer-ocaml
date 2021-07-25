module L = Low_discrepancy_sequence

(* s is in [0, 1); map it into [lower, upper) *)
let scale_sample s ~lower ~upper = ((upper -. lower) *. s) +. lower

(* Use quasi-monte-carlo to estimate integral of f(x) dx, for x in [lower, upper) *)
let integrate_1d f lower upper ~iterations =
  assert (lower < upper);
  let l = ref (L.create ~dimension:1) in
  let sample () =
    let l', s = L.step !l in
    l := l';
    scale_sample ~lower ~upper @@ L.Sample.(s.%{0})
  in
  let neg_sum = ref 0.0
  and pos_sum = ref 0.0 in
  for _ = 0 to iterations - 1 do
    let x = sample () in
    let y = f x in
    if y < 0.0 then neg_sum := !neg_sum +. y else pos_sum := !pos_sum +. y
  done;
  let sum = !pos_sum +. !neg_sum in
  let v_over_n = (upper -. lower) /. float_of_int iterations in
  v_over_n *. sum
;;

let check_float = Alcotest.(check (float 1e-3)) "within 1e-3"

let () =
  let test_case = Alcotest.test_case in
  Alcotest.run
    "low_discrepancy_sequence"
    [ ( "scale-sample"
      , let scale_sample s = scale_sample s ~lower:(-2.0) ~upper:3.0 in
        [ test_case "scale_sample 0 -> lower" `Quick (fun () ->
              check_float (-2.0) (scale_sample 0.0))
        ; test_case "scale_sample 1 -> upper" `Quick (fun () ->
              check_float 3.0 (scale_sample 1.0))
        ] )
    ; ( "1D-integrals"
      , [ test_case "sin(x) from 0 to 1" `Quick (fun () ->
              check_float 2.0 (integrate_1d sin 0.0 Float.pi ~iterations:1_000))
        ; test_case "sin(x) from -1 to 1" `Quick (fun () ->
              check_float 0.0 (integrate_1d sin (-1.0) 1.0 ~iterations:5_000))
        ; test_case "quarter circle -> pi/4" `Quick (fun () ->
              check_float
                (Float.pi /. 4.0)
                (integrate_1d (fun x -> sqrt (1.0 -. (x *. x))) 0.0 1.0 ~iterations:2_000))
        ] )
    ]
;;
