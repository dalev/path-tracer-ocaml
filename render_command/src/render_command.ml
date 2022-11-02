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

module Film : sig
  type t

  val create : width:int -> height:int -> output:string -> samples_per_pixel:int -> t
  val save_exn : t -> unit
  val write_pixel : t -> x:int -> y:int -> Color.t -> unit
end = struct
  type t =
    { raw_img : [ `Rgb ] Bimage.image_f64
    ; output : string
    ; samples_per_pixel : int
    ; filter_kernel : Filter_kernel.t
    }

  let color_space = Bimage.rgb
  let mkImage width height = Image.v Bimage.f64 color_space width height
  let pixel_radius = 1

  let create ~width ~height ~output ~samples_per_pixel =
    let raw_img = mkImage width height in
    let filter_kernel = Filter_kernel.Binomial.create ~order:5 ~pixel_radius in
    { raw_img; output; samples_per_pixel; filter_kernel }
  ;;

  let write_pixel t ~x ~y color =
    let width_minus_one = t.raw_img.Image.width - 1 in
    let height_minus_one = t.raw_img.Image.height - 1 in
    Filter_kernel.iter t.filter_kernel ~f:(fun ~dx ~dy weight ->
      let x = Int.clamp_exn (x + dx) ~min:0 ~max:width_minus_one
      and y = Int.clamp_exn (y + dy) ~min:0 ~max:height_minus_one in
      let incr ch v =
        let a = Image.get t.raw_img x y ch in
        Image.set t.raw_img x y ch (Caml.Float.fma weight v a)
      in
      let r, g, b = Color.to_rgb color in
      incr 0 r;
      incr 1 g;
      incr 2 b)
  ;;

  let save_exn t =
    let spp_inv = 1 // t.samples_per_pixel in
    let gamma f = Float.sqrt (f *. spp_inv) in
    let gamma_encoded_img = Bimage.Image.map_inplace gamma t.raw_img in
    match Bimage_unix.Stb.write t.output gamma_encoded_img with
    | Ok () -> ()
    | Error (#Bimage.Error.t as other) -> Bimage.Error.unwrap (Error other)
  ;;
end

module Make (Scene : sig
  val camera : Camera.t
  val intersect : Ray.t -> Hit.t option
  val background : Ray.t -> Color.t
end) =
struct
  let run { Args.width; height; max_bounces; samples_per_pixel; no_progress; output } =
    let film = Film.create ~width ~height ~output ~samples_per_pixel in
    let write_pixel = Film.write_pixel film in
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
    let render () =
      if no_progress
      then Integrator.render i ~update_progress:ignore
      else begin
        let total = width * height * samples_per_pixel in
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
    Film.save_exn film;
    printf "rendered in: %.3f ms\n" (Float.of_int63 elapsed *. 1e-6)
  ;;
end
