open! Core
open Core_bench
open Path_tracer

let unit_bbox = Bbox.create ~min:P3.origin ~max:(P3.create ~x:1.0 ~y:1.0 ~z:1.0)

let ray_hit, ray_miss =
  let o = P3.create ~x:(-5.0) ~y:0.5 ~z:0.5 in
  let hit = V3.unit_x in
  let miss = V3.unit_y in
  (Ray.create o hit, Ray.create o miss)

let t_min = 0.0

let t_max = 10.0

let is_hit ray =
  Bbox.is_hit
    (Sys.opaque_identity unit_bbox)
    (Sys.opaque_identity ray) ~t_min ~t_max

let () =
  Core.Command.run
    (Bench.make_command
       [
         Bench.Test.create ~name:"bbox hit" (fun () -> assert (is_hit ray_hit));
         Bench.Test.create ~name:"bbox miss" (fun () ->
             assert (not (is_hit ray_miss)));
       ])
