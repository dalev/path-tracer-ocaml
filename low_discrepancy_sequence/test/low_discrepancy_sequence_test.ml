module L = Low_discrepancy_sequence.Simple

(* s is in [0, 1); map it into [lower, upper) *)
let scale_sample s ~lower ~upper = ((upper -. lower) *. s) +. lower

(* Use quasi-monte-carlo to estimate integral of f(x) dx, for x in [lower, upper) *)
let integrate_1d f lower upper ~iterations =
  assert (lower < upper);
  let l = L.create ~dimensions:1 in
  let sample offset = scale_sample ~lower ~upper @@ L.get l ~offset ~dimension:0 in
  let sum = ref 0.0
  and c = ref 0.0 in
  for i = 0 to iterations - 1 do
    let y = (f @@ sample i) -. !c in
    let t = !sum +. y in
    c := t -. !sum -. y;
    sum := t
  done;
  let v_over_n = (upper -. lower) /. float_of_int iterations in
  v_over_n *. !sum
;;

let check_float ~within =
  let msg = "within " ^ string_of_float within in
  Alcotest.(check (float within)) msg
;;

let () =
  let test_case = Alcotest.test_case in
  Alcotest.run
    "low_discrepancy_sequence"
    [ ( "scale-sample"
      , let scale_sample s = scale_sample s ~lower:(-2.0) ~upper:3.0 in
        let within = 1e-3 in
        [ test_case "scale_sample 0 -> lower" `Quick (fun () ->
              check_float ~within (-2.0) (scale_sample 0.0))
        ; test_case "scale_sample 1 -> upper" `Quick (fun () ->
              check_float ~within 3.0 (scale_sample 1.0))
        ] )
    ; ( "1D-integrals"
      , [ test_case "sin(x) from 0 to pi = 2" `Quick (fun () ->
              check_float
                ~within:1e-3
                2.0
                (integrate_1d sin 0.0 Float.pi ~iterations:1_000))
        ; test_case "sin(x) from -1 to 1 = 0" `Quick (fun () ->
              check_float ~within:1e-3 0.0 (integrate_1d sin (-1.0) 1.0 ~iterations:5_000))
        ; test_case "quarter circle = pi/4" `Quick (fun () ->
              check_float
                ~within:1e-3
                (Float.pi /. 4.0)
                (integrate_1d (fun x -> sqrt (1.0 -. (x *. x))) 0.0 1.0 ~iterations:2_000))
        ; test_case "exp(x) from 0 to 3 = e^3 - 1" `Quick (fun () ->
              check_float
                ~within:0.03
                (Float.expm1 3.0)
                (integrate_1d Float.exp 0.0 3.0 ~iterations:2_000))
        ] )
    ]
;;
