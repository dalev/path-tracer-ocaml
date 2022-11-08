open! Core
open Core_bench
module Q = Path_tracer.Quaternion
module V3 = Path_tracer.V3

let u () = Random.float_range (-10.0) 10.0

let random_v () =
  let x = u ()
  and y = u ()
  and z = u () in
  V3.create ~x ~y ~z
;;

let random_q () = Q.create (u ()) (random_v ())

let () =
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create_group
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
       ])
;;
