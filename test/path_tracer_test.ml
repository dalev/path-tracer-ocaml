open! Base
open Path_tracer

let unit_bbox = Bbox.create ~min:P3.origin ~max:(P3.create ~x:1.0 ~y:1.0 ~z:1.0)

let ray_hit, ray_miss =
  let o = P3.create ~x:(-5.0) ~y:0.5 ~z:0.5 in
  let hit = V3.unit_x in
  let miss = V3.unit_y in
  (Ray.create o hit, Ray.create o miss)

let () =
  Alcotest.run "intersection"
    [ ( "Bbox.is_hit"
      , [ Alcotest.test_case "ray towards / t_max > 5" `Quick (fun () ->
              Alcotest.(check bool)
                "..." true
                (Bbox.is_hit unit_bbox ray_hit ~t_min:0.0 ~t_max:5.01) )
        ; Alcotest.test_case "ray towards / t_max < 5" `Quick (fun () ->
              Alcotest.(check bool)
                "..." false
                (Bbox.is_hit unit_bbox ray_hit ~t_min:0.0 ~t_max:4.99) )
        ; Alcotest.test_case "ray away" `Quick (fun () ->
              Alcotest.(check bool)
                "..." false
                (Bbox.is_hit unit_bbox ray_miss ~t_min:0.0 ~t_max:1000.0) ) ] )
    ]
