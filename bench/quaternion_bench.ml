open! Core
open Core_bench
module Q = Path_tracer.Quaternion
module V3 = Path_tracer.V3
module P3 = Path_tracer.P3
module S = Path_tracer.Shader_space
module Ray = Path_tracer.Ray

let u () = Random.float_range (-10.0) 10.0

let random_v () =
  let x = u ()
  and y = u ()
  and z = u () in
  V3.create ~x ~y ~z
;;

let random_p () = P3.of_v3 @@ random_v ()
let random_q () = Q.create (u ()) (random_v ())
let random_ss () = S.create (V3.normalize @@ random_v ()) (random_p ())
let random_ray () = Ray.create (random_p ()) (random_v ())

let quaternion =
  Bench.Test.create_group
    ~name:"quaternion"
    [ Bench.Test.create
        ~name:"mul"
        (let q = random_q ()
         and q' = random_q () in
         fun () -> Sys.opaque_identity (Q.mul q q'))
    ; Bench.Test.create
        ~name:"conj"
        (let q = random_q () in
         fun () -> Sys.opaque_identity (Q.conj q))
    ]
;;

let shader_space =
  Bench.Test.create_group
    ~name:"shader_space"
    [ Bench.Test.create
        ~name:"omega_i"
        (let ss = random_ss ()
         and ray = random_ray () in
         fun () -> Sys.opaque_identity (S.omega_i ss ray))
    ; Bench.Test.create
        ~name:"rotate"
        (let ss = random_ss ()
         and v = random_v () in
         fun () -> Sys.opaque_identity (S.rotate ss v))
    ; Bench.Test.create
        ~name:"rotate_inv"
        (let ss = random_ss ()
         and v = random_v () in
         fun () -> Sys.opaque_identity (S.rotate_inv ss v))
    ; Bench.Test.create
        ~name:"create"
        (let p = random_p ()
         and v = V3.normalize @@ random_v () in
         fun () -> Sys.opaque_identity (S.create v p))
    ]
;;

let () =
  Random.self_init ();
  Command_unix.run @@ Bench.make_command [ quaternion; shader_space ]
;;
