open! Core
open Core_bench
open Path_tracer

let unit_bbox = Bbox.create ~min:P3.origin ~max:(P3.create ~x:1.0 ~y:1.0 ~z:1.0)

let unit_sphere =
  let material = Material.lambertian (Texture.solid Color.black) in
  Sphere.create ~material ~center:P3.origin ~radius:1.0
;;

let ray_hit, ray_miss =
  let o = P3.create ~x:(-5.0) ~y:0.5 ~z:0.5 in
  let hit = V3.unit_x in
  let miss = V3.unit_y in
  Ray.create o hit, Ray.create o miss
;;

let t_min = 0.0
let t_max = 10.0

let is_hit ray =
  Bbox.is_hit (Sys.opaque_identity unit_bbox) (Sys.opaque_identity ray) ~t_min ~t_max
;;

let bbox_mem b pt = Bbox.mem (Sys.opaque_identity b) (Sys.opaque_identity pt)

let sphere_intersect s ray =
  Sphere.intersect (Sys.opaque_identity s) (Sys.opaque_identity ray) ~t_min ~t_max
  |> Option.is_some
;;

let () =
  Command_unix.run
  @@ Bench.make_command
       [ Bench.Test.create_group
           ~name:"bbox-intersect"
           [ Bench.Test.create ~name:"hit" (fun () -> assert (is_hit ray_hit))
           ; Bench.Test.create ~name:"miss" (fun () -> assert (not (is_hit ray_miss)))
           ]
       ; Bench.Test.create_group
           ~name:"bbox-mem"
           (let pt_in = P3.create ~x:0.5 ~y:0.5 ~z:0.5 in
            let pt_out = P3.create ~x:2.0 ~y:(-0.5) ~z:0.0 in
            [ Bench.Test.create ~name:"inside" (fun () ->
                assert (bbox_mem unit_bbox pt_in))
            ; Bench.Test.create ~name:"outside" (fun () ->
                assert (not (bbox_mem unit_bbox pt_out)))
            ])
       ; Bench.Test.create_group
           ~name:"sphere-intersect"
           [ Bench.Test.create ~name:"hit" (fun () ->
               assert (sphere_intersect unit_sphere ray_hit))
           ; Bench.Test.create ~name:"miss" (fun () ->
               assert (not (sphere_intersect unit_sphere ray_miss)))
           ]
       ]
;;
