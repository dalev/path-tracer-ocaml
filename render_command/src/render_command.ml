open Base
open Stdio
open Path_tracer
module Image = Bimage.Image

let color_space = Bimage.rgb
let mkImage width height = Image.v Bimage.f64 color_space width height

module Args = struct
  type t =
    { width : int
    ; height : int
    ; samples_per_pixel : int
    ; output : string
    ; no_progress : bool
    ; max_bounces : int
    }

  let parse ?(specs = []) () =
    let width = ref 600 in
    let height = ref !width in
    let spp = ref 1 in
    let file = ref "output.png" in
    let no_progress = ref false in
    let max_bounces = ref 4 in
    let usage_msg =
      Printf.sprintf "Defaults: width = %d, height = %d, output = %s" !width !height !file
    in
    let specs =
      specs
      @ Caml.Arg.
          [ "-width", Set_int width, "<integer> image width"
          ; "-height", Set_int height, "<integer> image height"
          ; "-samples-per-pixel", Set_int spp, "<integer> samples-per-pixel"
          ; "-o", Set_string file, "<file> output file"
          ; "-no-progress", Set no_progress, "suppress progress monitor"
          ; "-max-bounces", Set_int max_bounces, "<integer> max ray bounces"
          ]
    in
    Caml.Arg.parse
      specs
      (fun (_ : string) -> failwith "No anonymous arguments expected")
      usage_msg;
    { width = !width
    ; height = !height
    ; samples_per_pixel = !spp
    ; output = !file
    ; no_progress = !no_progress
    ; max_bounces = !max_bounces
    }
  ;;
end

let with_elapsed_time f =
  let start = Time_now.nanoseconds_since_unix_epoch () in
  let x = f () in
  let stop = Time_now.nanoseconds_since_unix_epoch () in
  let elapsed = Int63.(stop - start) in
  elapsed, x
;;

module Make (Scene : sig
  val camera : Camera.t
  val intersect : Ray.t -> Hit.t option
  val background : Ray.t -> Color.t
end) =
struct
  let run
      ?pool
      { Args.width; height; max_bounces; samples_per_pixel; no_progress; output }
    =
    let img = mkImage width height in
    let write_pixel ~x ~y color =
      let r, g, b = Color.to_rgb color in
      Image.set img x y 0 r;
      Image.set img x y 1 g;
      Image.set img x y 2 b
    in
    let i =
      Integrator.create
        ~width
        ~height
        ~write_pixel
        ~max_bounces
        ~samples_per_pixel
        ~intersect:Scene.intersect
        ~background:Scene.background
        ~camera:Scene.camera
        ~diffuse_plus_light:Pdf.diffuse
    in
    let elapsed =
      fst
      @@ with_elapsed_time (fun () ->
             if no_progress
             then Integrator.render i ?pool ~update_progress:ignore
             else (
               let total = Integrator.count_tiles i in
               let p =
                 let open Progress.Line in
                 list
                   [ spinner ()
                   ; elapsed ()
                   ; bar ~style:`ASCII total
                   ; count_to total
                   ; spacer 4
                   ]
               in
               let module C = Domainslib.Chan in
               let c = C.make_bounded 8 in
               Progress.with_reporter p (fun report ->
                   let update_progress () = C.send c 1 in
                   let d =
                     Caml.Domain.spawn (fun () ->
                         let remaining = ref total in
                         while !remaining > 0 do
                           let m = C.recv c in
                           remaining := !remaining - m;
                           report m
                         done)
                   in
                   Integrator.render i ~update_progress;
                   Caml.Domain.join d)))
    in
    let () =
      match Bimage_io.write output img with
      | Ok () -> ()
      | Error (`File_not_found f) -> printf "File not found: %s" f
      | Error (#Bimage.Error.t as other) -> Bimage.Error.unwrap (Error other)
    in
    printf "rendered in: %.3f ms\n" (Float.of_int63 elapsed *. 1e-6)
  ;;
end
