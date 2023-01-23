open Base
open Stdio
open Path_tracer
module Image = Bimage.Image

module Args = struct
  type t =
    { width : int
    ; height : int
    ; samples_per_pixel : int
    ; output : string
    ; no_progress : bool
    ; max_bounces : int
    }

  let term =
    let open Cmdliner in
    let docs = Manpage.s_options in
    let dimension =
      let doc = "image dimensions" in
      Arg.(
        required
        & opt (some (pair int int)) None
        & info [ "d"; "dimension" ] ~docs ~doc ~docv:"WIDTH,HEIGHT")
    in
    let output =
      let doc = "write image to $(docv)" in
      Arg.(
        value & opt string "output.png" & info [ "o"; "output" ] ~docs ~doc ~docv:"PATH")
    in
    let spp =
      let doc = "trace $(docv) camera rays per pixel" in
      Arg.(value & opt int 1 & info [ "samples-per-pixel" ] ~docs ~doc ~docv:"INT")
    in
    let no_progress =
      let doc = "suppress progress bar" in
      Arg.(value & flag & info [ "no-progress" ] ~docs ~doc)
    in
    let max_bounces =
      let doc = "max ray bounces" in
      Arg.(value & opt int 8 & info [ "max-ray-bounces" ] ~docs ~doc ~docv:"INT")
    in
    let mk (width, height) samples_per_pixel output no_progress max_bounces =
      { width; height; samples_per_pixel; output; no_progress; max_bounces }
    in
    Term.(const mk $ dimension $ spp $ output $ no_progress $ max_bounces)
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
  let run { Args.width; height; max_bounces; samples_per_pixel; no_progress; output } =
    let image = Image.v Bimage.f64 Bimage.rgb width height in
    let save_exn () =
      match Bimage_unix.Stb.write output image with
      | Ok () -> ()
      | Error (#Bimage.Error.t as other) -> Bimage.Error.unwrap (Error other)
    in
    let i =
      Integrator.create
        ~width
        ~height
        ~image
        ~max_bounces
        ~samples_per_pixel
        ~intersect:Scene.intersect
        ~background:Scene.background
        ~camera:Scene.camera
        ~diffuse_plus_light:Pdf.diffuse
    in
    let render () =
      if no_progress
      then Integrator.render i ~update_progress:ignore
      else begin
        let total = width * height in
        let p =
          let open Progress.Line in
          list
            [ spinner ()
            ; elapsed ()
            ; bar ~style:`ASCII total
            ; percentage_of total
            ; spacer 4
            ]
        in
        let config =
          let min_interval = Some (Progress.Duration.of_sec 0.2) in
          Progress.Config.v ~min_interval ()
        in
        Progress.with_reporter ~config p (fun report ->
          Integrator.render i ~update_progress:report)
      end
    in
    let elapsed, () = with_elapsed_time render in
    save_exn ();
    printf "rendered in: %.3f ms\n" (Float.of_int63 elapsed *. 1e-6)
  ;;
end
