open Core_bench
module L = Low_discrepancy_sequence

let bench_immutable_step ~dimension =
  let seq = L.create ~dimension in
  Core.Staged.stage (fun () -> Sys.opaque_identity @@ L.step (Sys.opaque_identity seq))
;;

let () =
  Bench.make_command
    [ Bench.Test.create_indexed ~name:"immutable step" ~args:[ 1; 10; 100 ] (fun i ->
          bench_immutable_step ~dimension:i)
    ]
  |> Core.Command.run
;;
